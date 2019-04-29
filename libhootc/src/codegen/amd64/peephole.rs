use std::collections::HashMap;
use std::collections::hash_map::Entry;

use super::Function;
use super::instr::*;
use crate::bitvec::BitVec;
use crate::codegen::label::Label;
use crate::il::IlRegister;
use crate::log::Log;

fn for_used_labels<F: FnMut (Label) -> ()>(instr: &InstructionKind, mut f: F) {
    match *instr {
        InstructionKind::Jump(label) => {
            f(label);
        },
        InstructionKind::JumpConditional(_, label) => {
            f(label);
        },
        _ => {}
    };
}

fn remove_unused_labels(code: &mut Vec<Instruction>, log: &mut Log) {
    let mut label_rc = HashMap::new();

    for instr in code.iter() {
        for_used_labels(&instr.node, |label| {
            match label_rc.entry(label) {
                Entry::Vacant(e) => {
                    e.insert(1);
                },
                Entry::Occupied(mut e) => {
                    *e.get_mut() += 1;
                }
            };
        });
    };

    code.drain_filter(|instr| {
        match instr.node {
            InstructionKind::Label(label) => {
                if let Some(&rc) = label_rc.get(&label) {
                    instr.rc = rc;
                    false
                } else {
                    log_writeln!(log, "Removing unused label {}", label);
                    true
                }
            },
            InstructionKind::RemovableNop => true,
            _ => false
        }
    });
}

fn find_global_regs(code: &Vec<Instruction>) -> BitVec<IlRegister> {
    let mut global = BitVec::new();
    let mut defined = BitVec::new();

    for instr in code.iter() {
        if let InstructionKind::Label(_) = instr.node {
            defined.clear();
        } else {
            instr.node.for_srcs(|src| {
                if !defined.get(src) {
                    global.set(src, true);
                };
            });
            instr.node.for_dests(|dest| {
                defined.set(dest, true);
            });
        };
    };

    global
}

fn recalculate_rc(code: &mut Vec<Instruction>) {
    let global_regs = find_global_regs(code);

    let mut reg_rcs = HashMap::new();
    let mut eflags_rc = 0;

    for instr in code.iter_mut().rev() {
        if let InstructionKind::Label(_) = instr.node {
            reg_rcs.clear();
            eflags_rc = 0;
        } else {
            let mut new_rc: u16 = 0;

            if instr.node.sets_eflags() {
                new_rc = new_rc.saturating_add(eflags_rc);
                eflags_rc = 0;
            };
            instr.node.for_dests(|dest| {
                if global_regs.get(dest) {
                    new_rc = u16::max_value();
                } else if let Some(&rc) = reg_rcs.get(&dest) {
                    new_rc = new_rc.saturating_add(rc);
                };

                reg_rcs.remove(&dest);
            });

            instr.rc = new_rc;

            if instr.node.uses_eflags() {
                eflags_rc = eflags_rc.saturating_add(1);
            };
            instr.node.for_srcs(|src| {
                if !global_regs.get(src) {
                    match reg_rcs.entry(src) {
                        Entry::Vacant(e) => {
                            e.insert(1);
                        },
                        Entry::Occupied(mut e) => {
                            let new_rc = e.get().saturating_add(1);
                            *e.get_mut() = new_rc;
                        }
                    };
                };
            });
        };
    };
}

fn pre_ra_peephole_1(instrs: &mut [Instruction], log: &mut Log) -> bool {
    match instrs {
        [i0, ..] => match &mut i0.node {
            &mut InstructionKind::IMul(
                sz,
                XDest::Reg(DestRegister(RealRegister::None, Some(src), Some(dst))),
                XSrc::Imm(imm)
            ) => {
                log_writeln!(log, "Turning IMUL {} = {} * {} into IMULI", dst, src, imm);

                i0.node = InstructionKind::IMulI(
                    sz,
                    DestRegister(RealRegister::None, None, Some(dst)),
                    XSrc::Reg(SrcRegister(RealRegister::None, Some(src))),
                    imm
                );

                true
            },
            _ => false
        },
        _ => false
    }
}

fn can_add(imm: i32, shift: u8, disp: i32) -> bool {
    ((imm << shift) >> shift) == imm && disp.checked_add(imm << shift).is_some()
}

fn pre_ra_peephole_2(instrs: &mut [Instruction], log: &mut Log) -> bool {
    match instrs {
        [i0, i1, ..] => match (&mut i0.node, &mut i1.node) {
            (
                &mut InstructionKind::Add(
                    sz0,
                    XDest::Reg(DestRegister(RealRegister::None, Some(src0_0), Some(dst0))),
                    XSrc::Reg(SrcRegister(RealRegister::None, Some(src0_1)))
                ),
                &mut InstructionKind::Add(
                    sz1,
                    XDest::Reg(DestRegister(RealRegister::None, Some(src1), Some(dst1))),
                    XSrc::Imm(imm)
                )
            ) if src1 == dst0 && (sz0 == RegisterSize::QWord || sz0 == RegisterSize::DWord)
                && sz0 == sz1 && i0.rc == 1 => {
                log_writeln!(log, "Turning {} = {} + {} + {} into an LEA", dst1, src0_0, src0_1, imm);

                i0.node = InstructionKind::RemovableNop;
                i1.node = InstructionKind::LoadEffectiveAddress(
                    sz0,
                    DestRegister(RealRegister::None, None, Some(dst1)),
                    MemArg {
                        base: Some(SrcRegister(RealRegister::None, Some(src0_0))),
                        index: Some(SrcRegister(RealRegister::None, Some(src0_1))),
                        scale: 1,
                        displacement: imm
                    }
                );
                true
            },
            (
                &mut InstructionKind::Add(
                    sz0,
                    XDest::Reg(DestRegister(RealRegister::None, Some(src0), Some(dst0))),
                    XSrc::Imm(imm)
                ),
                &mut InstructionKind::Add(
                    sz1,
                    XDest::Reg(DestRegister(RealRegister::None, Some(src1_0), Some(dst1))),
                    XSrc::Reg(SrcRegister(RealRegister::None, Some(src1_1)))
                )
            ) if (src1_0 == dst0 || src1_1 == dst0) && (sz0 == RegisterSize::QWord || sz0 == RegisterSize::DWord)
                && sz0 == sz1 && i0.rc == 1 => {
                let src1 = if src1_0 == dst0 {
                    src1_1
                } else {
                    src1_0
                };

                log_writeln!(log, "Turning {} = {} + {} + {} into an LEA", dst1, src0, imm, src1);

                i0.node = InstructionKind::RemovableNop;
                i1.node = InstructionKind::LoadEffectiveAddress(
                    sz0,
                    DestRegister(RealRegister::None, None, Some(dst1)),
                    MemArg {
                        base: Some(SrcRegister(RealRegister::None, Some(src0))),
                        index: Some(SrcRegister(RealRegister::None, Some(src1))),
                        scale: 1,
                        displacement: imm
                    }
                );
                true
            },
            (
                &mut InstructionKind::ShlI(
                    sz0,
                    XDest::Reg(DestRegister(RealRegister::None, Some(src0), Some(dst0))),
                    shift
                ),
                &mut InstructionKind::Add(
                    sz1,
                    XDest::Reg(DestRegister(RealRegister::None, Some(src1), Some(dst1))),
                    XSrc::Imm(imm)
                )
            ) if (sz0 == RegisterSize::QWord || sz0 == RegisterSize::DWord) && sz0 == sz1
                && dst0 == src1 && shift <= MemArg::max_scale_shift()
                && i0.rc == 1 => {
                log_writeln!(log, "Turning {} = ({} << {}) + {} into an LEA", dst1, src0, shift, imm);

                i0.node = InstructionKind::RemovableNop;
                i1.node = InstructionKind::LoadEffectiveAddress(
                    sz0,
                    DestRegister(RealRegister::None, None, Some(dst1)),
                    MemArg {
                        base: None,
                        index: Some(SrcRegister(RealRegister::None, Some(src0))),
                        scale: 1 << shift,
                        displacement: imm
                    }
                );

                true
            },
            (
                &mut InstructionKind::ShlI(
                    sz0,
                    XDest::Reg(DestRegister(RealRegister::None, Some(src0), Some(dst0))),
                    shift
                ),
                &mut InstructionKind::Add(
                    sz1,
                    XDest::Reg(DestRegister(RealRegister::None, Some(src1_0), Some(dst1))),
                    XSrc::Reg(SrcRegister(RealRegister::None, Some(src1_1)))
                )
            ) if (sz0 == RegisterSize::QWord || sz0 == RegisterSize::DWord) && sz0 == sz1
                && (dst0 == src1_0 || dst0 == src1_1) && shift <= MemArg::max_scale_shift()
                && i0.rc == 1 => {
                let src1 = if src1_0 == dst0 {
                    src1_1
                } else {
                    src1_0
                };

                log_writeln!(log, "Turning {} = ({} << {}) + {} into an LEA", dst1, src0, shift, src1);

                i0.node = InstructionKind::RemovableNop;
                i1.node = InstructionKind::LoadEffectiveAddress(
                    sz0,
                    DestRegister(RealRegister::None, None, Some(dst1)),
                    MemArg {
                        base: Some(SrcRegister(RealRegister::None, Some(src1))),
                        index: Some(SrcRegister(RealRegister::None, Some(src0))),
                        scale: 1 << shift,
                        displacement: 0
                    }
                );

                true
            },
            (
                &mut InstructionKind::LoadEffectiveAddress(
                    sz0,
                    DestRegister(RealRegister::None, None, Some(dst0)),
                    MemArg { base, index, scale, displacement }
                ),
                &mut InstructionKind::Add(
                    sz1,
                    XDest::Reg(DestRegister(RealRegister::None, Some(src1), Some(dst1))),
                    XSrc::Imm(imm)
                )
            ) if sz0 == sz1 && dst0 == src1 && can_add(imm, 0, displacement) && i0.rc == 1 => {
                log_writeln!(log, "Merging {} = {} + {} into previous LEA", dst1, src1, imm);

                i0.node = InstructionKind::LoadEffectiveAddress(
                    sz0,
                    DestRegister(RealRegister::None, None, Some(dst1)),
                    MemArg {
                        base,
                        index,
                        scale,
                        displacement: displacement + imm
                    }
                );
                i1.node = InstructionKind::RemovableNop;

                true
            },
            (
                &mut InstructionKind::Add(
                    sz0,
                    XDest::Reg(DestRegister(RealRegister::None, Some(src0), Some(dst0))),
                    XSrc::Imm(imm)
                ),
                &mut InstructionKind::LoadEffectiveAddress(
                    sz1,
                    DestRegister(RealRegister::None, None, Some(dst1)),
                    MemArg {
                        base: Some(SrcRegister(RealRegister::None, Some(src1))),
                        index,
                        scale,
                        displacement
                    }
                )
            ) if sz0 == sz1 && src1 == dst0 && can_add(imm, 1, displacement) && i0.rc == 1 => {
                log_writeln!(log, "Merging {} = {} + {} into next LEA with base {}", dst0, src0, imm, dst0);

                i0.node = InstructionKind::RemovableNop;
                i1.node = InstructionKind::LoadEffectiveAddress(
                    sz1,
                    DestRegister(RealRegister::None, None, Some(dst1)),
                    MemArg {
                        base: Some(SrcRegister(RealRegister::None, Some(src0))),
                        index,
                        scale,
                        displacement: displacement + imm
                    }
                );

                true
            },
            (
                &mut InstructionKind::Add(
                    sz0,
                    XDest::Reg(DestRegister(RealRegister::None, Some(src0), Some(dst0))),
                    XSrc::Imm(imm)
                ),
                &mut InstructionKind::LoadEffectiveAddress(
                    sz1,
                    DestRegister(RealRegister::None, None, Some(dst1)),
                    MemArg {
                        base,
                        index: Some(SrcRegister(RealRegister::None, Some(src1))),
                        scale,
                        displacement
                    }
                )
            ) if sz0 == sz1 && src1 == dst0 && can_add(imm, scale.trailing_zeros() as u8, displacement) && i0.rc == 1 => {
                log_writeln!(log, "Merging {} = {} + {} into next LEA with index {} * {}", dst0, src0, imm, dst0, scale);

                i0.node = InstructionKind::RemovableNop;
                i1.node = InstructionKind::LoadEffectiveAddress(
                    sz1,
                    DestRegister(RealRegister::None, None, Some(dst1)),
                    MemArg {
                        base,
                        index: Some(SrcRegister(RealRegister::None, Some(src0))),
                        scale,
                        displacement: displacement + (imm * (scale as i32))
                    }
                );

                true
            },
            (
                &mut InstructionKind::Jump(lbl0),
                &mut InstructionKind::Label(lbl1)
            ) if lbl0 == lbl1 => {
                log_writeln!(log, "Eliminating redundant jump to following label {}", lbl0);

                i0.node = InstructionKind::RemovableNop;

                if i1.rc == 1 {
                    log_writeln!(log, "  Also eliminating now-unused label {}", lbl0);
                    i1.node = InstructionKind::RemovableNop;
                };

                true
            },
            _ => false
        },
        _ => false
    }
}

fn pre_ra_peephole_3(instrs: &mut [Instruction], _log: &mut Log) -> bool {
    match instrs {
        [i0, i1, i2, ..] => match (&mut i0.node, &mut i1.node, &mut i2.node) {
            _ => false
        },
        _ => false
    }
}

fn pre_ra_peephole_4(instrs: &mut [Instruction], log: &mut Log) -> bool {
    match instrs {
        [i0, i1, i2, i3, ..] => match (&mut i0.node, &mut i1.node, &mut i2.node, &mut i3.node) {
            (
                &mut InstructionKind::SetCondition(
                    cond,
                    XDest::Reg(DestRegister(RealRegister::None, None, Some(dst0)))
                ),
                &mut InstructionKind::And(
                    RegisterSize::DWord,
                    XDest::Reg(DestRegister(RealRegister::None, Some(src1), Some(dst1))),
                    XSrc::Imm(1)
                ),
                &mut InstructionKind::Test(
                    RegisterSize::DWord,
                    XSrc::Reg(SrcRegister(RealRegister::None, Some(src2_0))),
                    XSrc::Reg(SrcRegister(RealRegister::None, Some(src2_1)))
                ),
                &mut InstructionKind::JumpConditional(Condition::Equal, label)
            ) if src1 == dst0 && src2_0 == dst1 && src2_1 == dst1
                && i0.rc == 1 && i1.rc == 2 && i2.rc == 1 => {
                log_writeln!(log, "Simplifying setcc/je to {} through {} into jnc", label, dst0);

                i0.node = InstructionKind::RemovableNop;
                i1.node = InstructionKind::RemovableNop;
                i2.node = InstructionKind::RemovableNop;
                i3.node = InstructionKind::JumpConditional(cond.reverse(), label);

                true
            },
            (
                &mut InstructionKind::SetCondition(
                    cond,
                    XDest::Reg(DestRegister(RealRegister::None, None, Some(dst0)))
                ),
                &mut InstructionKind::And(
                    RegisterSize::DWord,
                    XDest::Reg(DestRegister(RealRegister::None, Some(src1), Some(dst1))),
                    XSrc::Imm(1)
                ),
                &mut InstructionKind::Test(
                    RegisterSize::DWord,
                    XSrc::Reg(SrcRegister(RealRegister::None, Some(src2_0))),
                    XSrc::Reg(SrcRegister(RealRegister::None, Some(src2_1)))
                ),
                &mut InstructionKind::JumpConditional(Condition::NotEqual, label)
            ) if src1 == dst0 && src2_0 == dst1 && src2_1 == dst1
                && i0.rc == 1 && i1.rc == 2 && i2.rc == 1 => {
                log_writeln!(log, "Simplifying setcc/jne to {} through {} into jcc", label, dst0);

                i0.node = InstructionKind::RemovableNop;
                i1.node = InstructionKind::RemovableNop;
                i2.node = InstructionKind::RemovableNop;
                i3.node = InstructionKind::JumpConditional(cond, label);

                true
            },
            _ => false
        },
        _ => false
    }
}

pub fn do_pre_ra_peephole(func: &mut Function, log: &mut Log) {
    log_writeln!(log, "\n===== PRE REGISTER ALLOCATION PEEPHOLES ON {} =====\n", func.sym);

    loop {
        remove_unused_labels(&mut func.instrs, log);
        recalculate_rc(&mut func.instrs);

        let mut changed = false;

        for i in 0..func.instrs.len() {
            let code = &mut func.instrs[i..];

            changed = pre_ra_peephole_4(code, log) || changed;
            changed = pre_ra_peephole_3(code, log) || changed;
            changed = pre_ra_peephole_2(code, log) || changed;
            changed = pre_ra_peephole_1(code, log) || changed;
        };

        if !changed {
            break;
        };
    };
}
