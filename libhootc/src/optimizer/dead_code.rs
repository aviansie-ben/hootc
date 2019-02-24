use std::collections::VecDeque;
use std::io::Write;

use super::analysis::LivenessGraph;
use super::flow_graph::FlowGraph;
use crate::bitvec::BitVec;
use crate::il::{IlBlockId, IlFunction, IlInstructionKind, IlOperand};

pub fn eliminate_dead_blocks(func: &mut IlFunction, cfg: &mut FlowGraph<IlBlockId>, w: &mut Write) -> usize {
    writeln!(w, "\n===== DEAD BLOCK ELIMINATION =====\n").unwrap();

    let mut reachable = BitVec::new();
    let mut worklist = VecDeque::new();

    reachable.set(func.block_order[0], true);
    worklist.push_back(func.block_order[0]);

    while let Some(next) = worklist.pop_front() {
        for &succ in cfg.get(next).edges.iter() {
            if !reachable.set(succ, true) {
                worklist.push_back(succ);
            };
        };
    };

    let blocks = &mut func.blocks;
    let num_eliminated = func.block_order.drain_filter(|&mut id| {
        if !reachable.get(id) {
            writeln!(w, "Eliminating dead block {}", id).unwrap();

            blocks.remove(&id);
            if cfg.get(id).returns {
                cfg.remove_return_edge(id);
            };

            for succ in cfg.remove_node(id).edges {
                if let Some(succ) = cfg.try_get_mut(succ) {
                    succ.rev_edges.remove_item(&id);
                };
            };

            true
        } else {
            false
        }
    }).count();

    if num_eliminated != 0 {
        writeln!(w, "\n===== AFTER DEAD BLOCK ELIMINATION =====\n\n{}", func).unwrap();
    };

    num_eliminated
}

pub fn eliminate_dead_stores(func: &mut IlFunction, liveness: &mut LivenessGraph, w: &mut Write) -> usize {
    writeln!(w, "\n===== DEAD STORE ELIMINATION =====\n").unwrap();

    let mut num_eliminated = 0;

    for &id in func.block_order.iter() {
        let block = func.blocks.get_mut(&id).unwrap();
        let live_regs = liveness.get_mut(id);

        block.end_instr.node.for_operands(|o| if let IlOperand::Register(reg) = *o {
            live_regs.insert(reg);
        });

        for instr in block.instrs.iter_mut().rev() {
            if let Some(target) = instr.node.target() {
                if !live_regs.remove(&target) && instr.node.can_dead_store_eliminate() {
                    writeln!(w, "Removing dead store to {} in {}", target, id).unwrap();

                    instr.node = IlInstructionKind::Nop;
                };
            };

            instr.node.for_operands(|o| if let IlOperand::Register(reg) = *o {
                live_regs.insert(reg);
            });
        };

        num_eliminated += block.instrs.drain_filter(|instr| matches!(instr.node, IlInstructionKind::Nop)).count();
    };

    if num_eliminated != 0 {
        writeln!(w, "\n===== AFTER DEAD STORE ELIMINATION =====\n\n{}", func).unwrap();
    };

    num_eliminated
}
