use std::collections::HashMap;
use std::mem;

use itertools::Itertools;

use crate::il::*;
use crate::log::Log;
use crate::sym::SymId;

pub struct InlineSite {
    loc: (IlBlockId, usize),
    sym: SymId,
    blocks: Vec<IlBlock>,
    regs: Vec<(IlRegister, IlRegisterInfo)>,
    param_regs: Vec<IlRegister>,
    return_reg: IlRegister,
    spans: IlSpanMap
}

const MAX_INLINE_DEPTH: u8 = 5;
const MAX_INLINE_BLOCKS: usize = 5;

pub fn generate_inline_site(
    callee: &IlFunction,
    loc: (IlBlockId, usize)
) -> InlineSite {
    let mut block_id_map = HashMap::new();

    for (new_id, &old_id) in callee.block_order.iter().enumerate() {
        let new_id = new_id + 1;
        let new_id = IlBlockId::from(new_id);

        block_id_map.insert(old_id, new_id);
    };

    let return_reg = callee.reg_alloc.next();

    fn map_span(
        span: IlSpanId,
        func: &IlFunction,
        span_map: &mut HashMap<IlSpanId, IlSpanId>,
        spans: &mut IlSpanMap
    ) -> IlSpanId {
        if span == IlSpanId::dummy() {
            return IlSpanId::dummy();
        } else if let Some(&span) = span_map.get(&span) {
            return span;
        };

        let (span, inlined_at) = func.spans.get(span);
        let inlined_at = map_span(inlined_at, func, span_map, spans);

        spans.force_append(span, inlined_at)
    }

    let end_block = IlBlockId((callee.block_order.len() + 1) as u32);
    let mut blocks = vec![];
    let mut span_map = HashMap::new();
    let mut spans = IlSpanMap::new();

    blocks.push(IlBlock::new());

    for &old_id in callee.block_order.iter() {
        let mut block = callee.blocks[&old_id].clone();

        for instr in block.instrs.iter_mut() {
            instr.span = map_span(instr.span, callee, &mut span_map, &mut spans);

            match instr.node {
                IlInstructionKind::Call(_, _, _, ref mut depth) => {
                    *depth += 1;
                },
                _ => {}
            };
        };

        block.end_instr.span = map_span(block.end_instr.span, callee, &mut span_map, &mut spans);
        if let Some(target) = block.end_instr.node.target_block() {
            block.end_instr.node.set_target_block(block_id_map[&target]);
        };

        if let IlEndingInstructionKind::Return(o) = block.end_instr.node {
            block.instrs.push(IlInstruction::new(
                IlInstructionKind::Copy(return_reg, o),
                block.end_instr.span
            ));
            block.end_instr.node = IlEndingInstructionKind::Jump(end_block);
        };

        blocks.push(block);
    };

    blocks.push(IlBlock::new());

    let mut regs = vec![];

    for (reg, reg_info) in callee.reg_map.regs() {
        let reg_info = match *reg_info {
            IlRegisterInfo(IlRegisterType::Local(sym, span), ty) => {
                IlRegisterInfo(IlRegisterType::Local(sym, map_span(span, callee, &mut span_map, &mut spans)), ty)
            },
            IlRegisterInfo(IlRegisterType::Param(sym, _), ty) => {
                IlRegisterInfo(IlRegisterType::Local(sym, IlSpanId::dummy()), ty)
            },
            ref reg_info => reg_info.clone()
        };

        regs.push((reg, reg_info));
    };

    InlineSite {
        loc,
        sym: callee.sym,
        blocks,
        regs,
        param_regs: callee.reg_map.params().iter().cloned().collect_vec(),
        return_reg,
        spans
    }
}

pub fn do_inlining_decision_phase(prog: &IlProgram, func: &IlFunction, sites: &mut Vec<InlineSite>, log: &mut Log) {
    log_writeln!(log, "\n===== INLINING DECISIONS IN FUNCTION {} =====\n", func.sym);

    for &id in func.block_order.iter() {
        let block = &func.blocks[&id];

        for (i, instr) in block.instrs.iter().enumerate() {
            let callee = match instr.node {
                IlInstructionKind::Call(_, _, callee, depth) => {
                    if depth >= MAX_INLINE_DEPTH {
                        continue;
                    };

                    &prog.funcs[&callee]
                },
                _ => {
                    continue;
                }
            };

            if callee.block_order.len() > MAX_INLINE_BLOCKS {
                continue;
            };

            log_writeln!(log, "Will inline call to {} at {}:{}", callee.sym, id, i);

            sites.push(generate_inline_site(callee, (id, i)));
        };
    };
}

pub fn do_inlining_expansion_phase(func: &mut IlFunction, sites: &mut Vec<InlineSite>, log: &mut Log) {
    if sites.len() == 0 {
        return;
    };

    log_writeln!(log, "\n===== INLINING EXPANSION IN FUNCTION {} =====\n", func.sym);

    while let Some(site) = sites.pop() {
        log_writeln!(log, "Expanding call to {} at {}:{}", site.sym, site.loc.0, site.loc.1);

        let block_index = func.block_order.iter().enumerate().find_map(|(i, &id)| if id == site.loc.0 {
            Some(i)
        } else {
            None
        }).unwrap();

        let block = func.blocks.get_mut(&site.loc.0).unwrap();
        let inline_span = block.instrs[site.loc.1].span;

        log_writeln!(log, "  Inline site span is {}", inline_span);

        let next_id = func.next_block_id;
        func.next_block_id.0 += 1;

        log_writeln!(log, "  Splitting block into {} and {} across call", site.loc.0, next_id);

        let mut next_block = IlBlock::new();
        let mut split_instrs = block.instrs.drain(site.loc.1..);
        let call_instr = split_instrs.next().unwrap();

        next_block.id = next_id;
        next_block.instrs.extend(split_instrs);
        mem::swap(&mut block.end_instr, &mut next_block.end_instr);

        func.block_order.insert(block_index + 1, next_id);
        func.blocks.insert(next_id, next_block);

        let first_inline_block = func.next_block_id;
        func.next_block_id.0 += site.blocks.len() as u32;

        log_writeln!(log, "  Will inline in blocks {} through {}", first_inline_block, func.next_block_id);

        let first_inline_span = IlSpanId(func.spans.len() as u32);
        for (_, (span, inlined_at)) in site.spans.into_iter() {
            func.spans.force_append(span, if inlined_at == IlSpanId::dummy() {
                inline_span
            } else {
                inlined_at
            });
        };

        log_writeln!(log, "  Will inline using spans {} through {}", first_inline_span, IlSpanId(func.spans.len() as u32));

        let num_regs = site.regs.iter().map(|&(IlRegister(r), _)| r).max().map_or(0, |r| r + 1);
        let first_inline_reg = func.reg_alloc.allocate_many(num_regs);

        log_writeln!(log, "  Will inline using registers {} through {}", first_inline_reg, func.reg_alloc.next());

        for (mut reg, mut reg_info) in site.regs {
            match reg_info.0 {
                IlRegisterType::Local(_, ref mut old_span) => {
                    if *old_span == IlSpanId::dummy() {
                        *old_span = inline_span;
                    } else {
                        old_span.0 += first_inline_span.0;
                    };
                },
                _ => {}
            };
            reg.0 += first_inline_reg.0;
            func.reg_map.add_reg_info(reg, reg_info);
        };

        for (i, mut b) in site.blocks.into_iter().enumerate() {
            b.id.0 = first_inline_block.0 + i as u32;

            for instr in b.instrs.iter_mut() {
                if instr.span == IlSpanId::dummy() {
                    instr.span = inline_span;
                } else {
                    instr.span.0 += first_inline_span.0;
                };

                instr.node.for_operands_mut(|o| if let IlOperand::Register(ref mut r) = o {
                    r.0 += first_inline_reg.0;
                });

                if let Some(target) = instr.node.target_mut() {
                    if *target == site.return_reg {
                        *target = call_instr.node.target().unwrap();
                    } else {
                        target.0 += first_inline_reg.0;
                    };
                };
            };

            if b.end_instr.span == IlSpanId::dummy() {
                b.end_instr.span = inline_span;
            } else {
                b.end_instr.span.0 += first_inline_span.0;
            };

            b.end_instr.node.for_operands_mut(|o| if let IlOperand::Register(ref mut r) = o {
                r.0 += first_inline_reg.0;
            });

            if let Some(target) = b.end_instr.node.target_block() {
                b.end_instr.node.set_target_block(IlBlockId(
                    target.0 + first_inline_block.0
                ));
            };

            func.block_order.insert(block_index + 1 + i, b.id);
            func.blocks.insert(b.id, b);
        };

        let params_block = func.blocks.get_mut(&first_inline_block).unwrap();
        let mut params = site.param_regs.iter().cloned();

        call_instr.node.for_operands(|o| {
            params_block.instrs.push(IlInstruction::new(
                IlInstructionKind::Copy(
                    IlRegister(params.next().unwrap().0 + first_inline_reg.0), o.clone()
                ),
                call_instr.span
            ));
        });
    };

    log_writeln!(log, "\n===== AFTER INLINING EXPANSION =====\n\n{}", func);
}
