use std::collections::{HashMap, HashSet};
use std::collections::hash_map::Entry;
use std::mem;

use itertools::Itertools;

use crate::codegen::label::Label;
use crate::codegen::reg_alloc::RegisterAllocatorBase;
use crate::il::{IlRegister, IlRegisterAllocator, IlSpanId, IlType};
use crate::log::Log;
use crate::optimizer::analysis::{BlockLivenessInfo, LivenessGraph};
use crate::optimizer::flow_graph::FlowGraph;
use crate::sym::SymId;

use super::calling_convention::CallingConvention;
use super::instr::*;

struct RefCounts(HashMap<IlRegister, u32>);

impl RefCounts {
    fn new() -> RefCounts {
        RefCounts(HashMap::new())
    }

    fn inc(&mut self, r: IlRegister) -> u32 {
        match self.0.entry(r) {
            Entry::Occupied(mut e) => {
                *e.get_mut() += 1;
                *e.get_mut()
            },
            Entry::Vacant(e) => {
                e.insert(1);
                1
            }
        }
    }

    fn dec(&mut self, r: IlRegister) -> u32 {
        let rc = self.0.get_mut(&r).unwrap();

        assert_ne!(*rc, 0);

        *rc -= 1;
        *rc
    }

    fn get(&self, r: IlRegister) -> u32 {
        if let Some(&rc) = self.0.get(&r) {
            rc
        } else {
            0
        }
    }

    pub fn clear(&mut self) {
        self.0.clear();
    }
}

pub struct RegStackMap {
    map: HashMap<IlRegister, i32>,
    next: i32
}

impl RegStackMap {
    pub fn new() -> RegStackMap {
        RegStackMap { map: HashMap::new(), next: -8 }
    }

    pub fn get_or_alloc(&mut self, r: IlRegister) -> i32 {
        match self.map.entry(r) {
            Entry::Occupied(e) => *e.get(),
            Entry::Vacant(e) => {
                let off = self.next;
                self.next -= 8;

                e.insert(off);
                off
            }
        }
    }

    pub fn frame_size(&self) -> u32 {
        assert!(self.next < 0);
        -(self.next + 8) as u32
    }
}

fn split_blocks(code: impl Iterator<Item=Instruction>) -> (Vec<(Label, Vec<Instruction>)>, FlowGraph<Label>) {
    let mut blocks = vec![];
    let mut cfg = FlowGraph::new();
    let mut current_label = Label::dummy();
    let mut current_block = vec![];
    let mut falls_through = true;

    cfg.add_node(Label::dummy());

    for instr in code {
        match instr.node {
            InstructionKind::Label(label) => {
                blocks.push((current_label, current_block));

                if cfg.try_get(label).is_none() {
                    cfg.add_node(label);
                };

                if falls_through {
                    cfg.add_edge(current_label, label);
                };
                falls_through = true;

                current_label = label;
                current_block = vec![];
            },
            InstructionKind::Jump(label) => {
                if cfg.try_get(label).is_none() {
                    cfg.add_node(label);
                };

                cfg.add_edge(current_label, label);
                falls_through = false;
            },
            InstructionKind::JumpConditional(_, label) => {
                if cfg.try_get(label).is_none() {
                    cfg.add_node(label);
                };

                cfg.add_edge(current_label, label);
            },
            _ => {}
        };

        current_block.push(instr);
    };

    blocks.push((current_label, current_block));
    (blocks, cfg)
}

fn compute_liveness_effects(block: &Vec<Instruction>) -> BlockLivenessInfo {
    let mut gen = HashSet::new();
    let mut kill = HashSet::new();

    for instr in block.iter().rev() {
        instr.node.for_dests(|dest| {
            gen.remove(&dest);
            kill.insert(dest);
        });

        instr.node.for_srcs(|src| {
            kill.remove(&src);
            gen.insert(src);
        });
    };

    BlockLivenessInfo::new(gen, kill)
}

fn compute_liveness(blocks: &Vec<(Label, Vec<Instruction>)>, cfg: &FlowGraph<Label>, log: &mut Log) -> LivenessGraph<Label> {
    let mut liveness = LivenessGraph::new();
    let effects = blocks.iter().map(|&(label, ref instrs)| {
        (label, compute_liveness_effects(instrs))
    }).collect_vec();

    liveness.recompute_from_effects(effects, cfg, log);
    log_writeln!(log);
    liveness
}

pub struct RegisterAllocator<T: CallingConvention> {
    base: RegisterAllocatorBase<'static, RealRegister>,
    calling_convention: T,
    locked_registers: Vec<IlRegister>,
    reg_alloc: IlRegisterAllocator,
    rc: RefCounts,
    pub stack_map: RegStackMap
}

impl <T: CallingConvention> RegisterAllocator<T> {
    pub fn new(calling_convention: T, reg_alloc: IlRegisterAllocator) -> RegisterAllocator<T> {
        let base = RegisterAllocatorBase::new(calling_convention.allocatable_regs());
        RegisterAllocator {
            base,
            calling_convention,
            locked_registers: vec![],
            reg_alloc,
            rc: RefCounts::new(),
            stack_map: RegStackMap::new()
        }
    }

    fn unlock_all(&mut self) {
        self.locked_registers.clear();
    }

    fn spill_register(
        &mut self,
        virt: IlRegister,
        real: RealRegister,
        span: IlSpanId,
        code_out: &mut Vec<Instruction>,
        log: &mut Log
    ) {
        log_writeln!(log, "  Spilling {} (from {})", virt, real.name_qword());

        let instr = Instruction::new(
            InstructionKind::Mov(
                RegisterSize::QWord,
                XDest::Mem(MemArg {
                    base: None,
                    index: Some(SrcRegister::real(self.calling_convention.frame_pointer())),
                    scale: 1,
                    displacement: self.stack_map.get_or_alloc(virt)
                }),
                XSrc::Reg(SrcRegister(real, Some(virt)))
            ),
            span
        );

        log_writeln!(log, "    {}", instr.node);
        code_out.push(instr);
    }

    fn spill_all(&mut self, span: IlSpanId, code_out: &mut Vec<Instruction>, log: &mut Log) {
        for (real, virt) in self.base.spill_all().collect_vec() {
            self.spill_register(virt, real, span, code_out, log);
        };
    }

    fn undirty_all(&mut self, span: IlSpanId, code_out: &mut Vec<Instruction>, log: &mut Log) {
        for (real, virt) in self.base.undirty_all().collect_vec() {
            self.spill_register(virt, real, span, code_out, log);
        };
    }

    fn dump_all(&mut self) {
        self.base.spill_all();
    }

    fn unspill_register(
        &mut self,
        virt: IlRegister,
        real: RealRegister,
        span: IlSpanId,
        code_out: &mut Vec<Instruction>,
        log: &mut Log
    ) {
        let instr = Instruction::new(
            InstructionKind::Mov(
                RegisterSize::QWord,
                XDest::Reg(DestRegister(real, None, Some(virt))),
                XSrc::Mem(MemArg {
                    base: None,
                    index: Some(SrcRegister::real(self.calling_convention.frame_pointer())),
                    scale: 1,
                    displacement: self.stack_map.get_or_alloc(virt)
                })
            ),
            span
        );

        log_writeln!(log, "    {}", instr.node);
        code_out.push(instr);
    }

    fn lock(&mut self, src: IlRegister) {
        self.locked_registers.push(src);
    }

    fn allocate_and_lock(
        &mut self,
        src: IlRegister,
        needs_unspill: bool,
        span: IlSpanId,
        code_out: &mut Vec<Instruction>,
        log: &mut Log
    ) -> RealRegister {
        fn spill_heuristic(
            reg: &(IlRegister, RealRegister, bool),
            locked_registers: &[IlRegister]
        ) -> i32 {
            if locked_registers.contains(&reg.0) {
                i32::min_value()
            } else {
                0
            }
        }

        self.lock(src);
        if let Some(reg) = self.base.try_get(src) {
            reg
        } else if let Some(reg) = self.base.try_allocate(src, None) {
            log_writeln!(log, "  Allocated {} in {}", src, reg.name_qword());

            if needs_unspill {
                self.unspill_register(src, reg, span, code_out, log);
            };
            reg
        } else {
            let locked_registers = &self.locked_registers[..];
            let (reg, to_spill) = self.base.try_allocate_with_spill(
                src,
                |r| spill_heuristic(r, locked_registers)
            ).unwrap();

            if let Some(to_spill) = to_spill {
                self.spill_register(to_spill, reg, span, code_out, log);
            };

            log_writeln!(log, "  Allocated {} in {}", src, reg.name_qword());

            if needs_unspill {
                self.unspill_register(src, reg, span, code_out, log);
            };

            reg
        }
    }

    fn allocate_and_lock_in(
        &mut self,
        virt: IlRegister,
        real: RealRegister,
        span: IlSpanId,
        code_out: &mut Vec<Instruction>,
        log: &mut Log
    ) {
        if let Some((old_virt, old_dirty)) = self.base.try_get_in_real(real) {
            if old_virt != virt {
                self.base.free(old_virt);

                if let Some(old_real) = self.base.try_get(virt) {
                    assert!(self.base.try_move_to(virt, real));
                    assert!(self.base.try_allocate_in(old_virt, old_real));

                    log_writeln!(log, "  Swapping {} and {} to place {} in {} ({} now in {})", old_virt, virt, virt, real.name_qword(), old_virt, old_real.name_qword());

                    let instr = Instruction::new(
                        InstructionKind::XChgRR(
                            RegisterSize::QWord,
                            SrcRegister(real, Some(old_virt)),
                            SrcRegister(old_real, Some(virt))
                        ),
                        span
                    );

                    log_writeln!(log, "    {}", instr.node);
                    code_out.push(instr);
                } else {
                    assert!(self.base.try_allocate_in(virt, real));
                    self.lock(virt);

                    let spill_real = self.allocate_and_lock(old_virt, false, span, code_out, log);

                    log_writeln!(log, "  Moving {} (into {}) to allocate {} in {}", old_virt, spill_real.name_qword(), virt, real.name_qword());

                    let instr = Instruction::new(
                        InstructionKind::Mov(
                            RegisterSize::QWord,
                            XDest::Reg(DestRegister(spill_real, None, Some(old_virt))),
                            XSrc::Reg(SrcRegister(real, Some(old_virt)))
                        ),
                        span
                    );

                    log_writeln!(log, "    {}", instr.node);
                    code_out.push(instr);

                    self.unspill_register(virt, real, span, code_out, log);
                };

                if old_dirty {
                    self.base.mark_dirty(old_virt);
                };
            };
        } else if let Some(old_real) = self.base.try_get(virt) {
            assert!(self.base.try_move_to(virt, real));

            log_writeln!(log, "  Moving {} to {} (from {})", virt, real.name_qword(), old_real.name_qword());

            let instr = Instruction::new(
                InstructionKind::Mov(
                    RegisterSize::QWord,
                    XDest::Reg(DestRegister(real, None, Some(virt))),
                    XSrc::Reg(SrcRegister(old_real, Some(virt)))
                ),
                span
            );

            log_writeln!(log, "    {}", instr.node);
            code_out.push(instr);
        } else {
            assert!(self.base.try_allocate_in(virt, real));

            log_writeln!(log, "  Allocated {} in {}", virt, real.name_qword());

            self.unspill_register(virt, real, span, code_out, log);
        };

        self.lock(virt);
    }

    fn allocate_for_src_reg(
        &mut self,
        src: &mut SrcRegister,
        span: IlSpanId,
        code_out: &mut Vec<Instruction>,
        to_free: &mut Vec<IlRegister>,
        log: &mut Log
    ) {
        if src.0 == RealRegister::None {
            let src_virt = src.1.unwrap();
            src.0 = self.allocate_and_lock(src_virt, true, span, code_out, log);

            if self.rc.dec(src_virt) == 0 {
                to_free.push(src_virt);
            };
        } else {
            assert!(src.1.is_none());
        };
    }

    fn allocate_for_mem_arg(
        &mut self,
        mem_arg: &mut MemArg,
        span: IlSpanId,
        code_out: &mut Vec<Instruction>,
        to_free: &mut Vec<IlRegister>,
        log: &mut Log
    ) {
        if let Some(ref mut base) = mem_arg.base {
            self.allocate_for_src_reg(base, span, code_out, to_free, log);
        };
        if let Some(ref mut index) = mem_arg.index {
            self.allocate_for_src_reg(index, span, code_out, to_free, log);
        };
    }

    fn allocate_for_xsrc(
        &mut self,
        src: &mut XSrc,
        span: IlSpanId,
        code_out: &mut Vec<Instruction>,
        to_free: &mut Vec<IlRegister>,
        log: &mut Log
    ) {
        match *src {
            XSrc::Reg(ref mut src) => {
                self.allocate_for_src_reg(src, span, code_out, to_free, log);
            },
            XSrc::Mem(ref mut mem_arg) => {
                self.allocate_for_mem_arg(mem_arg, span, code_out, to_free, log);
            },
            _ => {}
        };
    }

    fn allocate_for_dest_reg(
        &mut self,
        dest: &mut DestRegister,
        span: IlSpanId,
        code_out: &mut Vec<Instruction>,
        _to_free: &mut Vec<IlRegister>,
        log: &mut Log
    ) {
        if dest.0 == RealRegister::None {
            let dest_virt = dest.2.unwrap();
            if let Some(src) = dest.1 {
                let src_real = self.allocate_and_lock(src, true, span, code_out, log);
                if src == dest_virt {
                    dest.0 = src_real;
                    self.rc.dec(src);
                } else if self.rc.dec(src) == 0 {
                    self.base.free(src);
                    assert!(self.base.try_move_to(dest_virt, src_real));
                    dest.0 = src_real;
                } else {
                    let tmp_virt = self.reg_alloc.allocate();

                    log_writeln!(log, "  Copying {} (into {}) to avoid overwriting it", src, tmp_virt);

                    let tmp_real = self.allocate_and_lock(tmp_virt, false, span, code_out, log);
                    let instr = Instruction::new(
                        InstructionKind::Mov(
                            RegisterSize::QWord,
                            XDest::Reg(DestRegister(tmp_real, None, Some(tmp_virt))),
                            XSrc::Reg(SrcRegister(src_real, Some(src)))
                        ),
                        span
                    );

                    log_writeln!(log, "    {}", instr.node);
                    code_out.push(instr);

                    self.base.free(tmp_virt);
                    assert!(self.base.try_move_to(dest_virt, tmp_real));

                    dest.0 = tmp_real;
                    dest.1 = Some(tmp_virt);
                };
            } else {
                dest.0 = self.allocate_and_lock(dest_virt, false, span, code_out, log);
            };

            self.base.mark_dirty(dest_virt);
        } else {
            assert!(dest.1.is_none());
            assert!(dest.2.is_none());
        };
    }

    fn allocate_for_xdest(
        &mut self,
        dest: &mut XDest,
        span: IlSpanId,
        code_out: &mut Vec<Instruction>,
        to_free: &mut Vec<IlRegister>,
        log: &mut Log
    ) {
        match *dest {
            XDest::Reg(ref mut dest) => {
                self.allocate_for_dest_reg(dest, span, code_out, to_free, log);
            },
            XDest::Mem(ref mut mem_arg) => {
                self.allocate_for_mem_arg(mem_arg, span, code_out, to_free, log);
            }
        };
    }

    fn try_commutate(
        &mut self,
        dest: &mut XDest,
        src: &mut XSrc,
        log: &mut Log
    ) {
        match (dest, src) {
            (&mut XDest::Reg(DestRegister(RealRegister::None, Some(ref mut src1), Some(ref mut dest))),
                &mut XSrc::Reg(SrcRegister(RealRegister::None, Some(ref mut src2)))) => {
                let should_swap = if src2 == dest && src1 != dest {
                    true
                } else if self.rc.get(*src2) == 1 && self.rc.get(*src1) != 1 {
                    true
                } else {
                    false
                };

                if should_swap {
                    log_writeln!(log, "  Swapping sources to avoid moving registers");
                    mem::swap(src1, src2);
                };
            },
            _ => {}
        };
    }

    fn allocate_for_instr(
        &mut self,
        mut instr: Instruction,
        code_out: &mut Vec<Instruction>,
        to_free: &mut Vec<IlRegister>,
        log: &mut Log
    ) {
        let span = instr.span;
        let mut will_emit = true;
        let mut will_unlock = true;

        log_writeln!(log, "{}", instr.node);

        match instr.node {
            InstructionKind::RemovableNop => {
                return;
            },
            InstructionKind::Label(_) => {
                self.spill_all(span, code_out, log);
            },
            InstructionKind::IMul(_, ref mut dest, ref mut src) => {
                self.try_commutate(dest, src, log);

                self.allocate_for_xsrc(src, span, code_out, to_free, log);
                self.allocate_for_xdest(dest, span, code_out, to_free, log);
            },
            InstructionKind::IMulI(_, ref mut dest, ref mut src, _) => {
                self.allocate_for_xsrc(src, span, code_out, to_free, log);
                self.allocate_for_dest_reg(dest, span, code_out, to_free, log);
            },
            InstructionKind::Add(_, ref mut dest, ref mut src) => {
                self.try_commutate(dest, src, log);

                self.allocate_for_xsrc(src, span, code_out, to_free, log);
                self.allocate_for_xdest(dest, span, code_out, to_free, log);
            },
            InstructionKind::Sub(_, ref mut dest, ref mut src) => {
                self.allocate_for_xsrc(src, span, code_out, to_free, log);
                self.allocate_for_xdest(dest, span, code_out, to_free, log);
            },
            InstructionKind::And(_, ref mut dest, ref mut src) => {
                self.try_commutate(dest, src, log);

                self.allocate_for_xsrc(src, span, code_out, to_free, log);
                self.allocate_for_xdest(dest, span, code_out, to_free, log);
            },
            InstructionKind::Test(_, ref mut src1, ref mut src2) => {
                self.allocate_for_xsrc(src1, span, code_out, to_free, log);
                self.allocate_for_xsrc(src2, span, code_out, to_free, log);
            },
            InstructionKind::Compare(_, ref mut src1, ref mut src2) => {
                self.allocate_for_xsrc(src1, span, code_out, to_free, log);
                self.allocate_for_xsrc(src2, span, code_out, to_free, log);
            },
            InstructionKind::Neg(_, ref mut dest) => {
                self.allocate_for_xdest(dest, span, code_out, to_free, log);
            },
            InstructionKind::Not(_, ref mut dest) => {
                self.allocate_for_xdest(dest, span, code_out, to_free, log);
            },
            InstructionKind::Mov(
                _,
                XDest::Reg(DestRegister(real, None, None)),
                XSrc::Reg(SrcRegister(RealRegister::None, Some(virt)))
            ) => {
                self.allocate_and_lock_in(virt, real, span, code_out, log);
                will_emit = false;
                will_unlock = false;
            },
            InstructionKind::Mov(
                _,
                XDest::Reg(DestRegister(RealRegister::None, None, Some(virt))),
                XSrc::Reg(SrcRegister(real, None))
            ) => {
                log_writeln!(log, "  Placing {} in {} without unspill", virt, real.name_qword());
                if self.base.try_get(virt).is_some() {
                    assert!(self.base.try_move_to(virt, real));
                } else {
                    assert!(self.base.try_allocate_in(virt, real));
                };
                will_emit = false;
            },
            InstructionKind::Mov(_, ref mut dest, ref mut src) => {
                self.allocate_for_xsrc(src, span, code_out, to_free, log);
                self.allocate_for_xdest(dest, span, code_out, to_free, log);
            },
            InstructionKind::Shl(_, ref mut dest, src) => {
                if let Some(src) = src {
                    self.allocate_and_lock_in(src, RealRegister::Rcx, span, code_out, log);
                };
                self.allocate_for_xdest(dest, span, code_out, to_free, log);
            },
            InstructionKind::ShlI(_, ref mut dest, _) => {
                self.allocate_for_xdest(dest, span, code_out, to_free, log);
            },
            InstructionKind::XChgRR(_, ref mut src1, ref mut src2) => {
                self.allocate_for_src_reg(src1, span, code_out, to_free, log);
                self.allocate_for_src_reg(src2, span, code_out, to_free, log);
            },
            InstructionKind::Push(_, ref mut src) => {
                self.allocate_for_xsrc(src, span, code_out, to_free, log);
            },
            InstructionKind::Pop(_, ref mut dest) => {
                self.allocate_for_xdest(dest, span, code_out, to_free, log);
            },
            InstructionKind::LoadEffectiveAddress(_, ref mut dest, ref mut mem_arg) => {
                self.allocate_for_dest_reg(dest, span, code_out, to_free, log);
                self.allocate_for_mem_arg(mem_arg, span, code_out, to_free, log);
            },
            InstructionKind::Jump(_) => {
                self.spill_all(span, code_out, log);
            },
            InstructionKind::JumpConditional(_, _) => {
                self.undirty_all(span, code_out, log);
            },
            InstructionKind::SetCondition(_, ref mut dest) => {
                self.allocate_for_xdest(dest, span, code_out, to_free, log);
            },
            InstructionKind::Call(_) => {
                // TODO Clear volatile registers
            },
            InstructionKind::Ret => {},
            InstructionKind::RegClear => {
                self.dump_all();
                will_emit = false;
            },
            InstructionKind::RegIn(ref regs) => {
                for &(real, virt) in regs.iter() {
                    self.allocate_and_lock_in(virt, real, span, code_out, log);
                };
                will_emit = false;
            }
        };

        if will_emit {
            log_writeln!(log, "  {}", instr.node);
            code_out.push(instr);
        };

        for r in to_free.drain(..) {
            log_writeln!(log, "  Freeing {}", r);
            self.base.free(r);
        };

        if will_unlock {
            self.unlock_all();
        };
    }

    fn allocate_for_block(
        &mut self,
        code_in: &mut Vec<Instruction>,
        code_out: &mut Vec<Instruction>,
        live_at_end: impl IntoIterator<Item=IlRegister>,
        log: &mut Log
    ) {
        self.rc.clear();

        for r in live_at_end.into_iter() {
            self.rc.inc(r);
        };

        for instr in code_in.iter() {
            instr.node.for_srcs(|r| {
                self.rc.inc(r);
            });
        };

        let mut to_free = vec![];

        for instr in code_in.drain(..) {
            self.allocate_for_instr(instr, code_out, &mut to_free, log);
        };

        log_writeln!(log, "; End of block");
        self.unlock_all();
        self.spill_all(IlSpanId::dummy(), code_out, log);
    }

    pub fn allocate(
        &mut self,
        func: SymId,
        code_in: Vec<Instruction>,
        params: impl Iterator<Item=(IlRegister, IlType)>,
        log: &mut Log
    ) -> Vec<Instruction> {
        log_writeln!(log, "\n===== REGISTER ALLOCATION FOR {} =====\n", func);

        let mut prologue = vec![];

        prologue.push(Instruction::new(
            InstructionKind::Push(
                RegisterSize::QWord,
                XSrc::Reg(SrcRegister::real(self.calling_convention.frame_pointer()))
            ),
            IlSpanId::dummy()
        ));
        prologue.push(Instruction::new(
            InstructionKind::Mov(
                RegisterSize::QWord,
                XDest::Reg(DestRegister::real(self.calling_convention.frame_pointer())),
                XSrc::Reg(SrcRegister::real(self.calling_convention.stack_pointer()))
            ),
            IlSpanId::dummy()
        ));
        prologue.push(Instruction::new(InstructionKind::RemovableNop, IlSpanId::dummy()));
        self.calling_convention.load_args(&mut prologue, params);

        let (blocks, cfg) = split_blocks(prologue.into_iter().chain(code_in.into_iter()));
        let liveness = compute_liveness(&blocks, &cfg, log);

        let mut code_out = vec![];

        for (label, mut instrs) in blocks {
            self.allocate_for_block(&mut instrs, &mut code_out, liveness.get(label).iter().cloned(), log);
        };

        if self.stack_map.frame_size() != 0 {
            code_out[2].node = InstructionKind::Sub(
                RegisterSize::QWord,
                XDest::Reg(DestRegister::real(self.calling_convention.stack_pointer())),
                XSrc::Imm(self.stack_map.frame_size() as i64)
            );
        };

        code_out.push(Instruction::new(
            InstructionKind::Mov(
                RegisterSize::QWord,
                XDest::Reg(DestRegister::real(self.calling_convention.stack_pointer())),
                XSrc::Reg(SrcRegister::real(self.calling_convention.frame_pointer()))
            ),
            IlSpanId::dummy()
        ));
        code_out.push(Instruction::new(
            InstructionKind::Pop(
                RegisterSize::QWord,
                XDest::Reg(DestRegister::real(self.calling_convention.frame_pointer()))
            ),
            IlSpanId::dummy()
        ));
        code_out.push(Instruction::new(InstructionKind::Ret, IlSpanId::dummy()));

        code_out
    }
}
