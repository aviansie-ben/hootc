use std::collections::HashMap;

use itertools::Itertools;

use super::eliminate_dead_blocks;
use super::flow_graph::FlowGraph;
use crate::il::{IlBlock, IlBlockId, IlEndingInstructionKind, IlFunction};
use crate::log::Log;

pub fn merge_blocks(func: &mut IlFunction, cfg: &mut FlowGraph<IlBlockId>, log: &mut Log) -> usize {
    log_writeln!(log, "\n===== BASIC BLOCK MERGING =====\n");

    let mut num_merged = 0;
    let mut i = 1;

    while i < func.block_order.len() {
        let prev_id = func.block_order[i - 1];
        let next_id = func.block_order[i];

        assert_ne!(prev_id, next_id);

        let prev = &func.blocks[&prev_id];

        let merged = if matches!(prev.end_instr.node, IlEndingInstructionKind::Nop) {
            if prev.instrs.is_empty() {
                log_writeln!(log, "Merging blocks {} and {} since {} is empty", prev_id, next_id, prev_id);

                for &old_pred_id in cfg.get(prev_id).rev_edges.iter() {
                    log_writeln!(log, "  Rewriting {} to target {}", old_pred_id, next_id);

                    let old_pred = func.blocks.get_mut(&old_pred_id).unwrap();

                    if old_pred.end_instr.node.target_block() == Some(prev_id) {
                        old_pred.end_instr.node.set_target_block(next_id);
                    };
                };

                func.block_order.remove(i - 1);
                cfg.merge_nodes_forward(prev_id, next_id);
                func.blocks.remove(&prev_id);

                true
            } else if cfg.get(next_id).rev_edges.len() == 1 {
                log_writeln!(log, "Merging blocks {} and {} since {} has one predecessor", prev_id, next_id, next_id);

                func.block_order.remove(i);
                cfg.merge_nodes_back(prev_id, next_id);

                let old_block = func.blocks.remove(&next_id).unwrap();
                let new_block = func.blocks.get_mut(&prev_id).unwrap();

                new_block.instrs.extend(old_block.instrs.into_iter());
                new_block.end_instr = old_block.end_instr;

                true
            } else {
                false
            }
        } else {
            false
        };

        if merged {
            num_merged += 1;
        } else {
            i += 1;
        };
    };

    if num_merged != 0 {
        log_writeln!(log, "\n===== AFTER BASIC BLOCK MERGING =====\n\n{}\n{}", func, cfg.pretty(func));
    };

    num_merged
}

pub fn eliminate_redundant_jumps(func: &mut IlFunction, cfg: &mut FlowGraph<IlBlockId>, log: &mut Log) -> usize {
    log_writeln!(log, "\n===== REDUNDANT JUMP ELIMINATION =====\n");

    let mut num_eliminated = 0;

    for (&prev_id, &next_id) in func.block_order.iter().tuple_windows() {
        let next = &func.blocks[&next_id];
        let effective_fallthrough = if next.instrs.is_empty() {
            match next.end_instr.node {
                IlEndingInstructionKind::Jump(effective_fallthrough) => Some(effective_fallthrough),
                _ => None
            }
        } else {
            None
        };

        let prev = func.blocks.get_mut(&prev_id).unwrap();

        if prev.end_instr.node.is_unconditional_jump_to(next_id) {
            log_writeln!(log, "Eliminating redundant jump from {} to fallthrough {}", prev_id, next_id);

            prev.end_instr.node = IlEndingInstructionKind::Nop;
            num_eliminated += 1;
        } else if prev.end_instr.node.target_block() == Some(next_id) {
            log_writeln!(log, "Eliminating redundant conditional jump from {} to fallthrough {}", prev_id, next_id);

            prev.end_instr.node = IlEndingInstructionKind::Nop;
            cfg.remove_edge(prev_id, next_id);
            num_eliminated += 1;
        } else if effective_fallthrough.is_some() && prev.end_instr.node.target_block() == effective_fallthrough {
            let effective_fallthrough = effective_fallthrough.unwrap();
            log_writeln!(log, "Eliminating redundant conditional jump from {} to effective fallthrough {}", prev_id, effective_fallthrough);

            prev.end_instr.node = IlEndingInstructionKind::Nop;
            cfg.remove_edge(prev_id, effective_fallthrough);
            num_eliminated += 1;
        };
    };

    if num_eliminated != 0 {
        log_writeln!(log, "\n===== AFTER REDUNDANT JUMP ELIMINATION =====\n\n{}\n{}", func, cfg.pretty(func));
    };

    num_eliminated
}

pub fn propagate_end_instr(func: &mut IlFunction, cfg: &mut FlowGraph<IlBlockId>, log: &mut Log) -> usize {
    log_writeln!(log, "\n===== END INSTRUCTION PROPAGATION =====\n");

    let mut fallthrough = HashMap::new();

    for (&prev_id, &next_id) in func.block_order.iter().tuple_windows() {
        fallthrough.insert(prev_id, next_id);
    };

    let mut num_propagated = 0;
    let mut to_insert = vec![];

    loop {
        for (i, &prev_id) in func.block_order.iter().enumerate() {
            let prev = &func.blocks[&prev_id];

            match prev.end_instr.node {
                IlEndingInstructionKind::Nop => {},
                IlEndingInstructionKind::Jump(_) => {},
                _ => {
                    continue;
                }
            };

            let next_id = cfg.get(prev_id).edges[0];
            let next = &func.blocks[&next_id];

            let new_end_instr = if next.instrs.is_empty() && !matches!(next.end_instr.node, IlEndingInstructionKind::Nop) {
                next.end_instr.clone()
            } else {
                continue;
            };
            let span = new_end_instr.span;
            let can_fall_through = new_end_instr.node.can_fall_through();

            // Don't propagate if the target block is an empty infinite loop. That can result in the
            // optimizer itself entering an infinite loop, since it will keep thinking that further
            // optimization is possible based on a newly propagated instruction.
            if matches!(next.end_instr.node, IlEndingInstructionKind::Jump(tgt) if tgt == next_id) {
                continue;
            };

            log_writeln!(log, "Propagating block-ending instruction from {} to {}", next_id, prev_id);
            num_propagated += 1;

            func.blocks.get_mut(&prev_id).unwrap().end_instr = new_end_instr;

            let next_node = cfg.get(next_id);
            let new_succ = next_node.edges.clone();
            let new_returns = next_node.returns;

            cfg.clear_edges_from(prev_id);

            for succ in new_succ {
                cfg.add_edge(prev_id, succ);
            };

            if new_returns {
                cfg.add_return_edge(prev_id);
            };

            if can_fall_through {
                let ft_id = func.next_block_id;
                func.next_block_id = IlBlockId(ft_id.0 + 1);

                let actual_ft_id = fallthrough[&next_id];

                log_writeln!(log, "  Adding block {} to provide fallthrough edge to {}", ft_id, actual_ft_id);

                let mut ft_block = IlBlock::new();
                ft_block.id = ft_id;
                ft_block.end_instr.node = IlEndingInstructionKind::Jump(actual_ft_id);
                ft_block.end_instr.span = span;

                func.blocks.insert(ft_id, ft_block);

                cfg.add_node(ft_id);
                cfg.add_edge(ft_id, actual_ft_id);
                cfg.remove_edge(prev_id, actual_ft_id);
                cfg.add_edge(prev_id, ft_id);

                fallthrough.insert(prev_id, ft_id);
                to_insert.push((i + 1, ft_id));
            };
        };

        if !to_insert.is_empty() {
            while let Some((i, id)) = to_insert.pop() {
                func.block_order.insert(i, id);
            };
        } else {
            break;
        };
    };

    if num_propagated != 0 {
        log_writeln!(log, "\n===== AFTER END INSTRUCTION PROPAGATION =====\n\n{}\n{}", func, cfg.pretty(func));
    };

    num_propagated
}

pub fn rewrite_jump_targets(func: &mut IlFunction, cfg: &mut FlowGraph<IlBlockId>, log: &mut Log) -> usize {
    log_writeln!(log, "\n===== JUMP TARGET REWRITING =====\n");

    let mut num_rewritten = 0;

    for &prev_id in func.block_order.iter() {
        let next_id = if let Some(next_id) = func.blocks[&prev_id].end_instr.node.target_block() {
            next_id
        } else {
            continue;
        };

        let next = &func.blocks[&next_id];

        let actual_next_id = if next.instrs.is_empty() {
            match next.end_instr.node {
                IlEndingInstructionKind::Jump(actual_next_id) => actual_next_id,
                _ => {
                    continue;
                }
            }
        } else {
            continue;
        };

        if actual_next_id == next_id {
            continue;
        };

        let prev = func.blocks.get_mut(&prev_id).unwrap();

        log_writeln!(log, "Rewriting jump from {} to {} into a jump to {}", prev_id, next_id, actual_next_id);

        prev.end_instr.node.set_target_block(actual_next_id);
        cfg.remove_edge(prev_id, next_id);
        cfg.add_edge(prev_id, actual_next_id);
        num_rewritten += 1;
    };

    if num_rewritten != 0 {
        log_writeln!(log, "\n===== AFTER JUMP TARGET REWRITING =====\n\n{}\n{}", func, cfg.pretty(func));
    };

    num_rewritten
}

pub fn do_merge_blocks_group(func: &mut IlFunction, cfg: &mut FlowGraph<IlBlockId>, log: &mut Log) -> bool {
    eliminate_dead_blocks(func, cfg, log);

    let mut cont = merge_blocks(func, cfg, log) != 0;
    cont = eliminate_redundant_jumps(func, cfg, log) != 0 || cont;
    cont = propagate_end_instr(func, cfg, log) != 0 || cont;
    cont = rewrite_jump_targets(func, cfg, log) != 0 || cont;

    if !cont {
        return false;
    };

    while cont {
        eliminate_dead_blocks(func, cfg, log);
        cont = merge_blocks(func, cfg, log) != 0;
        cont = eliminate_redundant_jumps(func, cfg, log) != 0 || cont;
        cont = propagate_end_instr(func, cfg, log) != 0 || cont;
        cont = rewrite_jump_targets(func, cfg, log) != 0 || cont;
    };
    true
}
