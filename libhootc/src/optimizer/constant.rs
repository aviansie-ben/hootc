use std::collections::HashMap;
use std::collections::hash_map::Entry;
use std::mem;

use itertools::Itertools;
use smallvec::SmallVec;

use super::{do_merge_blocks_group, eliminate_dead_stores, eliminate_local_common_subexpressions};
use super::analysis::{AnalysisStructures, Def};
use crate::il::*;
use crate::log::Log;

fn try_fold_instr(instr: &IlInstructionKind) -> Option<IlConst> {
    use crate::il::IlConst::*;
    use crate::il::IlInstructionKind::*;
    use crate::il::IlOperand::*;
    Some(match *instr {
        NegI32(_, Const(I32(v))) => I32(v.wrapping_neg()),
        NotI1(_, Const(I1(v))) => I1(!v),
        NotI32(_, Const(I32(v))) => I32(!v),
        AddI32(_, Const(I32(l)), Const(I32(r))) => I32(l.wrapping_add(r)),
        SubI32(_, Const(I32(l)), Const(I32(r))) => I32(l.wrapping_sub(r)),
        MulI32(_, Const(I32(l)), Const(I32(r))) => I32(l.wrapping_mul(r)),
        ShlI32(_, Const(I32(l)), Const(I32(r))) => I32(l >> r),
        EqI1(_, Const(I1(l)), Const(I1(r))) => I1(l == r),
        EqI32(_, Const(I32(l)), Const(I32(r))) => I1(l == r),
        NeI1(_, Const(I1(l)), Const(I1(r))) => I1(l != r),
        NeI32(_, Const(I32(l)), Const(I32(r))) => I1(l != r),
        _ => return None
    })
}

pub fn propagate_and_fold_constants(func: &mut IlFunction, structs: &AnalysisStructures, log: &mut Log) -> usize {
    let defs = &structs.defs;

    log_writeln!(log, "\n===== CONSTANT PROPAGATION AND FOLDING =====\n");

    let mut num_replaced = 0;
    let mut consts = HashMap::new();

    for &id in func.block_order.iter() {
        consts.clear();

        'reg: for (&reg, defs) in defs.get(id) {
            if !defs.is_empty() {
                let Def(def_blk, def_off) = defs[0];

                if def_blk == IlBlockId::dummy() {
                    continue;
                };

                let instr = &func.blocks[&def_blk].instrs[def_off].node;
                let constant = if let IlInstructionKind::Copy(_, IlOperand::Const(ref c)) = *instr {
                    c.clone()
                } else {
                    continue;
                };

                for &Def(def_blk, def_off) in defs[1..].iter() {
                    if def_blk == IlBlockId::dummy() {
                        continue 'reg;
                    };

                    let instr = &func.blocks[&def_blk].instrs[def_off].node;
                    if let IlInstructionKind::Copy(_, IlOperand::Const(ref c)) = *instr {
                        if c != &constant {
                            continue 'reg;
                        };
                    } else {
                        continue 'reg;
                    };
                };

                consts.insert(reg, constant);
            };
        };

        let block = func.blocks.get_mut(&id).unwrap();

        for (i, instr) in block.instrs.iter_mut().enumerate() {
            instr.node.for_operands_mut(|o| if let IlOperand::Register(reg) = *o {
                if let Some(constant) = consts.get(&reg) {
                    log_writeln!(log, "Replaced {} on {}:{} with {}", reg, id, i, constant);
                    *o = IlOperand::Const(constant.clone());

                    num_replaced += 1;
                };
            });

            if let Some(target) = instr.node.target() {
                if let Some(fold_constant) = try_fold_instr(&instr.node) {
                    log_writeln!(log, "Folded {}:{} to {}", id, i, fold_constant);
                    instr.node = IlInstructionKind::Copy(target, IlOperand::Const(fold_constant));

                    num_replaced += 1;
                };

                if let IlInstructionKind::Copy(_, IlOperand::Const(ref constant)) = instr.node {
                    consts.insert(target, constant.clone());
                } else {
                    consts.remove(&target);
                }
            };
        };

        block.end_instr.node.for_operands_mut(|o| if let IlOperand::Register(reg) = *o {
            if let Some(constant) = consts.get(&reg) {
                log_writeln!(log, "Replaced {} on {}:end with {}", reg, id, constant);
                *o = IlOperand::Const(constant.clone());

                num_replaced += 1;
            };
        });
    };

    if num_replaced != 0 {
        log_writeln!(log, "\n===== AFTER CONSTANT FOLDING AND PROPAGATION =====\n\n{}", func);
    };

    num_replaced
}

pub fn propagate_copies_locally(func: &mut IlFunction, structs: &AnalysisStructures, log: &mut Log) -> usize {
    let ebbs = &structs.ebbs;

    log_writeln!(log, "\n===== LOCAL COPY PROPAGATION =====\n");

    let mut num_replaced = 0;
    let mut copies = HashMap::new();
    let mut rev_copies: HashMap<IlRegister, SmallVec<[IlRegister; 2]>> = HashMap::new();

    for ebb in ebbs.iter() {
        copies.clear();
        rev_copies.clear();
        for &id in ebb.blocks.iter() {
            let block = func.blocks.get_mut(&id).unwrap();

            for (i, instr) in block.instrs.iter_mut().enumerate() {
                instr.node.for_operands_mut(|o| if let IlOperand::Register(reg) = *o {
                    if let Some(copied_reg) = copies.get(&reg).cloned() {
                        log_writeln!(log, "Replaced {} on {}:{} with {}", reg, id, i, copied_reg);
                        *o = IlOperand::Register(copied_reg);

                        num_replaced += 1;
                    };
                });

                if let IlInstructionKind::Copy(target, IlOperand::Register(copied_reg)) = instr.node {
                    if let Some(old_reg) = copies.insert(target, copied_reg) {
                        let rev_copies = rev_copies.get_mut(&old_reg).unwrap();

                        rev_copies.remove(
                            rev_copies.iter().enumerate().find(|&(_, &r)| r == target).map(|(i, _)| i).unwrap()
                        );
                    };

                    match rev_copies.entry(copied_reg) {
                        Entry::Occupied(mut o) => {
                            o.get_mut().push(target);
                        },
                        Entry::Vacant(v) => {
                            v.insert(smallvec![target]);
                        }
                    };
                } else if let Some(target) = instr.node.target() {
                    copies.remove(&target);

                    if let Some(rev_copies) = rev_copies.remove(&target) {
                        for rev_copy in rev_copies {
                            copies.remove(&rev_copy);
                        };
                    };
                };
            };

            block.end_instr.node.for_operands_mut(|o| if let IlOperand::Register(reg) = *o {
                if let Some(copied_reg) = copies.get(&reg).cloned() {
                    log_writeln!(log, "Replaced {} on {}:end with {}", reg, id, copied_reg);
                    *o = IlOperand::Register(copied_reg);

                    num_replaced += 1;
                };
            });
        };
    };

    if num_replaced != 0 {
        log_writeln!(log, "\n===== AFTER LOCAL COPY PROPAGATION =====\n\n{}", func);
    };

    num_replaced
}

pub fn elide_copies(func: &mut IlFunction, structs: &AnalysisStructures, log: &mut Log) -> usize {
    log_writeln!(log, "\n===== COPY ELISION =====\n");

    let global_regs = structs.liveness.global_regs();
    let mut num_elided = 0;

    for &id in func.block_order.iter() {
        loop {
            let block = func.blocks.get_mut(&id).unwrap();

            let mut movable: Vec<(IlRegister, usize, &mut IlInstruction)> = vec![];
            let mut move_candidates: Vec<(
                IlRegister, IlRegister,
                usize, &mut IlInstruction,
                usize, &mut IlInstruction
            )> = vec![];

            for (i, instr) in block.instrs.iter_mut().enumerate() {
                if let IlInstructionKind::Copy(target, IlOperand::Register(src)) = instr.node {
                    let movable_instr = movable.drain_filter(|&mut (move_target, _, _)| {
                        move_target == src
                    }).next();

                    if let Some((_, j, movable_instr)) = movable_instr {
                        move_candidates.push((target, src, i, instr, j, movable_instr));
                    };
                } else {
                    movable.drain_filter(|&mut (_, _, ref mut movable_instr)| {
                        !movable_instr.node.can_move_across(&instr.node)
                    });
                    instr.node.for_operands(|o| match *o {
                        IlOperand::Register(reg) => {
                            move_candidates.drain_filter(|&mut (_, src, _, _, _, _)| src == reg);
                        },
                        _ => {}
                    });

                    if let Some(target) = instr.node.target() {
                        if !global_regs.get(target) {
                            let old_movable = movable.iter_mut()
                                .find(|&&mut (old_target, _, _)| old_target == target);

                            if let Some(old_movable) = old_movable {
                                old_movable.1 = i;
                                old_movable.2 = instr;
                            } else {
                                movable.push((target, i, instr));
                            };
                        };
                    };
                };
            };

            let num_elided_block = move_candidates.len();
            for (target, src, i, copy_instr, j, instr) in move_candidates {
                log_writeln!(log, "Moving {}:{} to {}:{} in order to avoid copy from {} to {}", id, j, id, i, src, target);

                copy_instr.node = IlInstructionKind::Nop;
                *instr.node.target_mut().unwrap() = target;

                mem::swap(instr, copy_instr);
            };

            if num_elided_block != 0 {
                num_elided += num_elided_block;
            } else {
                block.instrs.drain_filter(|instr| match instr.node {
                    IlInstructionKind::Nop => true,
                    _ => false
                });
                break;
            };
        };
    };

    if num_elided != 0 {
        log_writeln!(log, "\n===== AFTER COPY ELISION =====\n\n{}", func);
    };

    num_elided
}

fn calc_constant_jump_condition(instr: &IlEndingInstructionKind) -> Option<bool> {
    use crate::il::IlConst::*;
    use crate::il::IlEndingInstructionKind::*;
    use crate::il::IlOperand::*;
    Some(match *instr {
        JumpNonZeroI1(_, Const(I1(v))) => v,
        JumpZeroI1(_, Const(I1(v))) => !v,
        _ => return None
    })
}

pub fn simplify_constant_jump_conditions(
    func: &mut IlFunction,
    structs: &mut AnalysisStructures,
    log: &mut Log
) -> usize {
    let cfg = &mut structs.cfg;

    log_writeln!(log, "\n===== CONSTANT JUMP SIMPLIFICATION =====\n");

    let mut num_simplified = 0;

    for i in 0..func.block_order.len() {
        let id = func.block_order[i];

        let block = func.blocks.get_mut(&id).unwrap();

        match calc_constant_jump_condition(&block.end_instr.node) {
            Some(true) => {
                log_writeln!(log, "Turning always-taken conditional jump in {} into unconditional jump", id);
                cfg.remove_edge(id, func.block_order[i + 1]);
                block.end_instr.node = IlEndingInstructionKind::Jump(
                    block.end_instr.node.target_block().unwrap()
                );

                num_simplified += 1;
            },
            Some(false) => {
                log_writeln!(log, "Removing never-taken conditional jump in {}", id);
                cfg.remove_edge(id, block.end_instr.node.target_block().unwrap());
                block.end_instr.node = IlEndingInstructionKind::Nop;

                num_simplified += 1;
            },
            None => {}
        };
    };

    if num_simplified != 0 {
        log_writeln!(log, "\n===== AFTER CONSTANT JUMP SIMPLIFICATION =====\n\n{}\n{}", func, cfg.pretty(func));
        do_merge_blocks_group(func, cfg, log);
    };

    num_simplified
}

fn try_simplify_algebraically<F: FnMut (IlInstructionKind) -> ()>(
    id: IlBlockId,
    i: usize,
    instr: &mut IlInstructionKind,
    reg_alloc: &mut IlRegisterAllocator,
    reg_map: &mut IlRegisterMap,
    mut emit_extra: F,
    log: &mut Log
) -> bool {
    use crate::il::IlConst::*;
    use crate::il::IlInstructionKind::*;
    use crate::il::IlOperand::*;

    let mut updated = match *instr {
        AddI32(tgt, Const(I32(l)), Register(r)) => {
            log_writeln!(log, "Moved constant to RHS at {}:{}", id, i);
            *instr = IlInstructionKind::AddI32(tgt, Register(r), Const(I32(l)));
            true
        },
        SubI32(tgt, Register(l), Const(I32(r))) => {
            log_writeln!(log, "Canonicalized immediate subtraction into addition at {}:{}", id, i);
            *instr = IlInstructionKind::AddI32(tgt, Register(l), Const(I32(r.wrapping_neg())));
            true
        },
        MulI32(tgt, Const(I32(l)), Register(r)) => {
            log_writeln!(log, "Moved constant to RHS at {}:{}", id, i);
            *instr = IlInstructionKind::MulI32(tgt, Register(r), Const(I32(l)));
            true
        },
        EqI1(tgt, Const(I1(l)), Register(r)) => {
            log_writeln!(log, "Moved constant to RHS at {}:{}", id, i);
            *instr = IlInstructionKind::EqI1(tgt, Register(r), Const(I1(l)));
            true
        },
        EqI32(tgt, Const(I32(l)), Register(r)) => {
            log_writeln!(log, "Moved constant to RHS at {}:{}", id, i);
            *instr = IlInstructionKind::EqI32(tgt, Register(r), Const(I32(l)));
            true
        },
        NeI1(tgt, Const(I1(l)), Register(r)) => {
            log_writeln!(log, "Moved constant to RHS at {}:{}", id, i);
            *instr = IlInstructionKind::NeI1(tgt, Register(r), Const(I1(l)));
            true
        },
        NeI32(tgt, Const(I32(l)), Register(r)) => {
            log_writeln!(log, "Moved constant to RHS at {}:{}", id, i);
            *instr = IlInstructionKind::NeI32(tgt, Register(r), Const(I32(l)));
            true
        },
        _ => false
    };

    updated = (match *instr {
        AddI32(tgt, Register(l), Const(I32(0))) => {
            log_writeln!(log, "Collapsed {} + i32:0 => {} at {}:{}", l, l, id, i);
            *instr = IlInstructionKind::Copy(tgt, Register(l));
            true
        },
        SubI32(tgt, Register(l), Const(I32(0))) => {
            log_writeln!(log, "Collapsed {} - i32:0 => {} at {}:{}", l, l, id, i);
            *instr = IlInstructionKind::Copy(tgt, Register(l));
            true
        },
        SubI32(tgt, Const(I32(0)), Register(r)) => {
            log_writeln!(log, "Collapsed i32:0 - {} => -{} at {}:{}", r, r, id, i);
            *instr = IlInstructionKind::NegI32(tgt, Register(r));
            true
        },
        SubI32(tgt, Register(l), Register(r)) if l == r => {
            log_writeln!(log, "Collapsed {} - {} => i32:0 at {}:{}", l, l, id, i);
            *instr = IlInstructionKind::Copy(tgt, Const(I32(0)));
            true
        },
        MulI32(tgt, Register(l), Const(I32(0))) => {
            log_writeln!(log, "Collapsed {} * i32:0 => i32:0 at {}:{}", l, id, i);
            *instr = IlInstructionKind::Copy(tgt, Const(I32(0)));
            true
        },
        MulI32(tgt, Register(l), Const(I32(1))) => {
            log_writeln!(log, "Collapsed {} * i32:1 => {} at {}:{}", l, l, id, i);
            *instr = IlInstructionKind::Copy(tgt, Register(l));
            true
        },
        MulI32(tgt, Register(l), Const(I32(-1))) => {
            log_writeln!(log, "Collapsed {} * i32:-1 => -{} at {}:{}", l, l, id, i);
            *instr = IlInstructionKind::NegI32(tgt, Register(l));
            true
        },
        MulI32(tgt, Register(l), Const(I32(val))) if val.count_ones() == 1 => {
            let bits = val.trailing_zeros();
            log_writeln!(log, "Collapsed {} * {} => {} << {} at {}:{}", l, val, l, bits, id, i);
            *instr = IlInstructionKind::ShlI32(tgt, Register(l), Const(I32(bits as i32)));
            true
        },
        MulI32(tgt, Register(l), Const(I32(val))) if val.wrapping_neg().count_ones() == 1 => {
            let tmp = reg_alloc.allocate();
            reg_map.add_reg_info(tmp, IlRegisterInfo(IlRegisterType::Temp, IlType::I32));

            let bits = val.wrapping_neg().trailing_zeros();
            log_writeln!(log, "Collapsed {} * {} => -({} << {}) at {}:{}", l, val, l, bits, id, i);

            emit_extra(IlInstructionKind::ShlI32(tmp, Register(l), Const(I32(bits as i32))));
            *instr = IlInstructionKind::NegI32(tgt, Register(tmp));

            true
        },
        MulI32(tgt, Register(l), Const(I32(val))) if val.wrapping_sub(1).count_ones() == 1 => {
            let tmp = reg_alloc.allocate();
            reg_map.add_reg_info(tmp, IlRegisterInfo(IlRegisterType::Temp, IlType::I32));

            let bits = val.wrapping_sub(1).trailing_zeros();
            log_writeln!(log, "Collapsed {} * {} => ({} << {}) + {} at {}:{}", l, val, l, bits, l, id, i);

            emit_extra(IlInstructionKind::ShlI32(tmp, Register(l), Const(I32(bits as i32))));
            *instr = IlInstructionKind::AddI32(tgt, Register(tmp), Register(l));

            true
        },
        MulI32(tgt, Register(l), Const(I32(val))) if val.wrapping_neg().wrapping_sub(1).count_ones() == 1 => {
            let tmp1 = reg_alloc.allocate();
            let tmp2 = reg_alloc.allocate();

            reg_map.add_reg_info(tmp1, IlRegisterInfo(IlRegisterType::Temp, IlType::I32));
            reg_map.add_reg_info(tmp2, IlRegisterInfo(IlRegisterType::Temp, IlType::I32));

            let bits = val.wrapping_sub(1).trailing_zeros();
            log_writeln!(log, "Collapsed {} * {} => -(({} << {}) + {}) at {}:{}", l, val, l, bits, l, id, i);

            emit_extra(IlInstructionKind::ShlI32(tmp1, Register(l), Const(I32(bits as i32))));
            emit_extra(IlInstructionKind::AddI32(tmp2, Register(tmp1), Register(l)));
            *instr = IlInstructionKind::NegI32(tgt, Register(tmp2));

            true
        },
        MulI32(tgt, Register(l), Const(I32(val))) if val.wrapping_add(1).count_ones() == 1 => {
            let tmp = reg_alloc.allocate();
            reg_map.add_reg_info(tmp, IlRegisterInfo(IlRegisterType::Temp, IlType::I32));

            let bits = val.wrapping_add(1).trailing_zeros();
            log_writeln!(log, "Collapsed {} * {} => ({} << {}) - {} at {}:{}", l, val, l, bits, l, id, i);

            emit_extra(IlInstructionKind::ShlI32(tmp, Register(l), Const(I32(bits as i32))));
            *instr = IlInstructionKind::SubI32(tgt, Register(tmp), Register(l));

            true
        },
        MulI32(tgt, Register(l), Const(I32(val))) if val.wrapping_neg().wrapping_add(1).count_ones() == 1 => {
            let tmp = reg_alloc.allocate();
            reg_map.add_reg_info(tmp, IlRegisterInfo(IlRegisterType::Temp, IlType::I32));

            let bits = val.wrapping_neg().wrapping_add(1).trailing_zeros();
            log_writeln!(log, "Collapsed {} * {} => {} - ({} << {}) at {}:{}", l, val, l, bits, l, id, i);

            emit_extra(IlInstructionKind::ShlI32(tmp, Register(l), Const(I32(bits as i32))));
            *instr = IlInstructionKind::SubI32(tgt, Register(l), Register(tmp));

            true
        },
        ShlI32(tgt, Register(l), Const(I32(0))) => {
            log_writeln!(log, "Collapsed {} << i32:0 => {} at {}:{}", l, l, id, i);
            *instr = IlInstructionKind::Copy(tgt, Const(I32(1)));
            true
        },
        EqI1(tgt, Register(l), Register(r)) if l == r => {
            log_writeln!(log, "Collapsed {} == {} => i1:1 at {}:{}", l, l, id, i);
            *instr = IlInstructionKind::Copy(tgt, Const(I1(true)));
            true
        },
        EqI1(tgt, Register(l), Const(I1(true))) => {
            log_writeln!(log, "Collapsed {} == i1:1 => {} at {}:{}", l, l, id, i);
            *instr = IlInstructionKind::Copy(tgt, Register(l));
            true
        },
        EqI1(tgt, Register(l), Const(I1(false))) => {
            log_writeln!(log, "Collapsed {} == i1:0 => !{} at {}:{}", l, l, id, i);
            *instr = IlInstructionKind::NotI1(tgt, Register(l));
            true
        },
        EqI32(tgt, Register(l), Register(r)) if l == r => {
            log_writeln!(log, "Collapsed {} == {} => i1:1 at {}:{}", l, l, id, i);
            *instr = IlInstructionKind::Copy(tgt, Const(I1(true)));
            true
        },
        NeI1(tgt, Register(l), Const(I1(true))) => {
            log_writeln!(log, "Collapsed {} != i1:1 => !{} at {}:{}", l, l, id, i);
            *instr = IlInstructionKind::NotI1(tgt, Register(l));
            true
        },
        NeI1(tgt, Register(l), Const(I1(false))) => {
            log_writeln!(log, "Collapsed {} != i1:0 => {} at {}:{}", l, l, id, i);
            *instr = IlInstructionKind::Copy(tgt, Register(l));
            true
        },
        NeI1(tgt, Register(l), Register(r)) if l == r => {
            log_writeln!(log, "Collapsed {} != {} => i1:0 at {}:{}", l, l, id, i);
            *instr = IlInstructionKind::Copy(tgt, Const(I1(false)));
            true
        },
        NeI32(tgt, Register(l), Register(r)) if l == r => {
            log_writeln!(log, "Collapsed {} != {} => i1:0 at {}:{}", l, l, id, i);
            *instr = IlInstructionKind::Copy(tgt, Const(I1(false)));
            true
        },
        _ => false
    }) || updated;

    updated = (match *instr {
        Copy(tgt, Register(src)) if tgt == src => {
            log_writeln!(log, "Eliminated redundant copy from {} to {} at {}:{}", src, src, id, i);
            *instr = IlInstructionKind::Nop;
            true
        },
        _ => false
    }) || updated;

    updated
}

pub fn simplify_algebraically(func: &mut IlFunction, log: &mut Log) -> usize {
    log_writeln!(log, "\n===== ALGEBRAIC SIMPLIFICATION =====\n");

    let mut num_simplified = 0;
    let mut to_emit = vec![];

    for &id in func.block_order.iter() {
        let block = func.blocks.get_mut(&id).unwrap();

        for (i, instr) in block.instrs.iter_mut().enumerate() {
            let span = instr.span;
            let mut more_instr = vec![];
            let emit_extra = |instr| {
                more_instr.push(IlInstruction::new(instr, span));
            };

            if try_simplify_algebraically(id, i, &mut instr.node, &mut func.reg_alloc, &mut func.reg_map, emit_extra, log) {
                num_simplified += 1;

                if !more_instr.is_empty() {
                    to_emit.push((i, more_instr));
                };
            };
        };

        while let Some((i, more_instr)) = to_emit.pop() {
            block.instrs.splice(i..i, more_instr.into_iter());
        };
    };

    if num_simplified != 0 {
        log_writeln!(log, "\n===== AFTER ALGEBRAIC SIMPLIFICATION =====\n\n{}", func);
    };

    num_simplified
}

fn try_simplify_jump_condition(
    block: IlBlockId,
    cmp_ins: &IlInstructionKind,
    jmp_ins: &mut IlEndingInstructionKind,
    log: &mut Log
) -> bool {
    let (reverse, target, reg) = match *jmp_ins {
        IlEndingInstructionKind::JumpNonZeroI1(target, IlOperand::Register(reg)) => (false, target, reg),
        IlEndingInstructionKind::JumpZeroI1(target, IlOperand::Register(reg)) => (true, target, reg),
        _ => {
            return false;
        }
    };

    match *cmp_ins {
        IlInstructionKind::NotI1(cmp_reg, ref val) if cmp_reg == reg && val != &IlOperand::Register(reg) => {
            if reverse {
                log_writeln!(log, "{}: Replacing !i1 followed by jz.i1 with jnz.i1", block);
                *jmp_ins = IlEndingInstructionKind::JumpNonZeroI1(target, val.clone());
            } else {
                log_writeln!(log, "{}: Replacing !i1 followed by jnz.i1 with jz.i1", block);
                *jmp_ins = IlEndingInstructionKind::JumpZeroI1(target, val.clone());
            };
            true
        },
        _ => false
    }
}

pub fn simplify_jump_conditions(func: &mut IlFunction, log: &mut Log) -> usize {
    log_writeln!(log, "\n===== JUMP CONDITION SIMPLIFICATION =====\n");

    let mut num_simplified = 0;

    for &id in func.block_order.iter() {
        let block = func.blocks.get_mut(&id).unwrap();

        if !block.instrs.is_empty()
            && try_simplify_jump_condition(id, &block.instrs.last().unwrap().node, &mut block.end_instr.node, log) {
            num_simplified += 1;
        };
    };

    if num_simplified != 0 {
        log_writeln!(log, "\n===== AFTER JUMP CONDITION SIMPLIFICATION =====\n\n{}", func);
    };

    num_simplified
}

pub fn eliminate_tail_calls(func: &mut IlFunction, structs: &mut AnalysisStructures, log: &mut Log) -> usize {
    let cfg = &mut structs.cfg;

    log_writeln!(log, "\n===== TAIL CALL ELIMINATION =====\n");

    let mut num_eliminated = 0;
    let start_block = func.block_order.first().cloned().unwrap();

    for id in cfg.returning_nodes().iter().cloned().collect_vec() {
        let block = func.blocks.get_mut(&id).unwrap();
        let return_reg = if let IlEndingInstructionKind::Return(IlOperand::Register(reg)) = block.end_instr.node {
            reg
        } else {
            continue;
        };

        let (span, (tgt, params, callee)) = if let Some(last_instr) = block.instrs.last() {
            (last_instr.span, if let IlInstructionKind::Call(tgt, ref params, callee, _) = last_instr.node {
                (tgt, params, callee)
            } else {
                continue;
            })
        } else {
            continue;
        };

        if tgt != return_reg || callee != func.sym {
            continue;
        };

        log_writeln!(log, "Turning tail call to self in {} into a jump to {}", id, start_block);

        let params = params.clone();
        let first_param_temp = func.reg_alloc.allocate_many(params.len() as u32);

        block.instrs.pop();

        for (i, param) in params.into_iter().enumerate() {
            let tmp_reg = IlRegister(first_param_temp.0 + i as u32);
            let ty = param.data_type(&func.reg_map);

            func.reg_map.add_reg_info(tmp_reg, IlRegisterInfo(IlRegisterType::Temp, ty));
            block.instrs.push(IlInstruction::new(
                IlInstructionKind::Copy(tmp_reg, param),
                span
            ));
        };

        for (i, param_reg) in func.reg_map.params().iter().cloned().enumerate() {
            let tmp_reg = IlRegister(first_param_temp.0 + i as u32);
            block.instrs.push(IlInstruction::new(
                IlInstructionKind::Copy(param_reg, IlOperand::Register(tmp_reg)),
                span
            ));
        };

        block.end_instr.node = IlEndingInstructionKind::Jump(start_block);

        cfg.remove_return_edge(id);
        cfg.add_edge(id, start_block);

        num_eliminated += 1;
    };

    if num_eliminated != 0 {
        log_writeln!(log, "\n===== AFTER TAIL CALL ELIMINATION =====\n\n{}\n{}", func, cfg.pretty(func));
    };

    num_eliminated
}

pub fn do_constant_fold_group(
    func: &mut IlFunction,
    structs: &mut AnalysisStructures,
    log: &mut Log
) -> bool {
    structs.liveness.recompute_global_regs(func, log);
    elide_copies(func, structs, log);

    structs.ebbs.recompute(func, &structs.cfg, log);
    propagate_copies_locally(func, structs, log);

    structs.liveness.recompute_global_regs(func, log);
    structs.defs.recompute(func, &structs.cfg, structs.liveness.global_regs(), log);
    let mut cont = propagate_and_fold_constants(func, structs, log) != 0;

    cont = eliminate_local_common_subexpressions(func, structs, log) != 0 || cont;

    structs.liveness.recompute(func, &structs.cfg, log);
    cont = eliminate_dead_stores(func, &mut structs.liveness, log) != 0 || cont;

    cont = simplify_constant_jump_conditions(func, structs, log) != 0 || cont;
    cont = simplify_algebraically(func, log) != 0 || cont;
    cont = simplify_jump_conditions(func, log) != 0 || cont;
    cont = eliminate_tail_calls(func, structs, log) != 0 || cont;

    if !cont {
        return false;
    };

    while cont {
        structs.liveness.recompute_global_regs(func, log);
        elide_copies(func, structs, log);

        structs.ebbs.recompute(func, &structs.cfg, log);
        propagate_copies_locally(func, structs, log);

        structs.liveness.recompute_global_regs(func, log);
        structs.defs.recompute(func, &structs.cfg, structs.liveness.global_regs(), log);
        cont = propagate_and_fold_constants(func, structs, log) != 0;

        cont = eliminate_local_common_subexpressions(func, structs, log) != 0 || cont;

        structs.liveness.recompute(func, &structs.cfg, log);
        cont = eliminate_dead_stores(func, &mut structs.liveness, log) != 0 || cont;

        cont = simplify_constant_jump_conditions(func, structs, log) != 0 || cont;
        cont = simplify_algebraically(func, log) != 0 || cont;
        cont = simplify_jump_conditions(func, log) != 0 || cont;
        cont = eliminate_tail_calls(func, structs, log) != 0 || cont;
    };
    true
}
