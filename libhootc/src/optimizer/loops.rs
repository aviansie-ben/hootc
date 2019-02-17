use std::collections::HashMap;
use std::io::Write;
use std::mem;

use super::{eliminate_dead_stores, propagate_copies_locally};
use super::analysis::{Dominance, ExtendedBlocks, ReachingDefs, LivenessGraph, Loops};
use super::flow_graph::FlowGraph;
use crate::bitvec::BitVec;
use crate::il::*;

pub fn move_loop_invariant_code(
    func: &mut IlFunction,
    loops: &mut Loops,
    w: &mut Write
) -> usize {
    writeln!(w, "\n===== LOOP INVARIANT CODE MOTION =====\n").unwrap();

    let mut num_moved = 0;

    let mut to_move = vec![];
    let mut loop_writes = BitVec::new();
    let mut moved_const = HashMap::new();

    for l in loops.loops.iter() {
        loop_writes.clear();

        for id in l.blocks() {
            for i in func.blocks.get_mut(&id).unwrap().instrs.iter() {
                if let Some(target) = i.node.target() {
                    loop_writes.set(target, true);
                };
            };
        };

        for id in l.blocks() {
            moved_const.clear();

            for (i, ins) in func.blocks.get_mut(&id).unwrap().instrs.iter_mut().enumerate() {
                if !ins.node.can_move_from_loop() {
                    continue;
                };

                let mut is_const = true;
                ins.node.for_operands_mut(|o| {
                    match o {
                        IlOperand::Register(ref mut r) => {
                            if let Some(new_r) = moved_const.get(r) {
                                *r = *new_r;
                            } else if loop_writes.get(*r) {
                                is_const = false;
                            };
                        },
                        IlOperand::Const(_) => {}
                    }
                });

                if is_const {
                    if let Some(target) = ins.node.target() {
                        let reg = func.reg_alloc.allocate();
                        let ty = func.reg_map.get_reg_info(target).1;
                        func.reg_map.add_reg_info(reg, IlRegisterInfo(IlRegisterType::Temp, ty));

                        writeln!(w, "Moving loop-invariant instruction at {}:{} into pre-header {} using temp {}", id, i, l.pre_header, reg).unwrap();

                        let mut new_ins = IlInstructionKind::Copy(target, IlOperand::Register(reg));

                        mem::swap(&mut ins.node, &mut new_ins);
                        *new_ins.target_mut().unwrap() = reg;
                        to_move.push(IlInstruction::new(new_ins, ins.span));

                        moved_const.insert(target, reg);
                    } else {
                        writeln!(w, "Moving loop-invariant instruction at {}:{} into pre-header {}", id, i, l.pre_header).unwrap();
                        let mut new_ins = IlInstructionKind::Nop;

                        mem::swap(&mut ins.node, &mut new_ins);
                        to_move.push(IlInstruction::new(new_ins, ins.span));
                    };
                } else if let Some(target) = ins.node.target() {
                    moved_const.remove(&target);
                };
            };
        };

        if !to_move.is_empty() {
            num_moved += to_move.len();

            let pre_header = func.blocks.get_mut(&l.pre_header).unwrap();
            pre_header.instrs.extend(to_move.drain(..));
        };
    };

    if num_moved != 0 {
        writeln!(w, "\n===== AFTER LOOP INVARIANT CODE MOTION =====\n\n{}", func).unwrap();
    };

    num_moved
}

pub fn do_loop_opt_group(
    func: &mut IlFunction,
    cfg: &mut FlowGraph<IlBlockId>,
    liveness: &mut LivenessGraph,
    defs: &mut ReachingDefs,
    ebbs: &mut ExtendedBlocks,
    dom: &mut Dominance,
    loops: &mut Loops,
    w: &mut Write
) {
    dom.recompute(func, cfg, w);
    loops.recompute(func, cfg, dom, w);

    if loops.loops.is_empty() {
        return;
    };

    move_loop_invariant_code(func, loops, w);

    ebbs.recompute(func, cfg, w);
    liveness.recompute_global_regs(func, w);
    defs.recompute(func, cfg, liveness.global_regs(), w);
    propagate_copies_locally(func, ebbs, w);

    liveness.recompute(func, cfg, w);
    eliminate_dead_stores(func, liveness, w);
}
