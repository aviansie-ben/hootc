use std::mem;

use itertools::Itertools;

use super::ast;
use super::il::*;
use super::lex::Span;
use super::log::{Log, SharedLog};
use super::sym::{FunctionId, SymDef, SymDefKind, SymDefTable, SymId};
use super::types::{Type, TypeId, TypeTable};

pub fn map_to_il_type(ty: TypeId, types: &TypeTable) -> IlType {
    match *types.find_type(ty) {
        Type::Error => unreachable!(),
        Type::Bool => IlType::I1,
        Type::I32 => IlType::I32,
        Type::Tuple(ref tys) => match &tys[..] {
            [] => IlType::Void,
            [ ty ] => map_to_il_type(*ty, types),
            _ => unimplemented!()
        },
        Type::Func(_, _) => IlType::Addr,
        Type::FuncKnown(_, _) => IlType::Void,
        Type::Undecided(_) => unreachable!()
    }
}

pub struct IlBuilder<'a> {
    pub current_block: IlBlock,
    pub func: IlFunction,
    log: SharedLog<'a>
}

impl <'a> IlBuilder<'a> {
    pub fn new(sym: SymId, log: SharedLog) -> IlBuilder {
        IlBuilder {
            current_block: IlBlock::new(),
            func: IlFunction::new(sym),
            log
        }
    }

    pub fn relink_block_target(&mut self, from: IlBlockId, to: IlBlockId) {
        self.func.blocks.get_mut(&from).unwrap().end_instr.node.set_target_block(to);
    }

    pub fn allocate_register(&mut self) -> IlRegister {
        self.func.reg_alloc.allocate()
    }

    pub fn allocate_temp(&mut self, t: IlType) -> IlRegister {
        let reg = self.allocate_register();

        self.func.reg_map.add_reg_info(reg, IlRegisterInfo(IlRegisterType::Temp, t));
        reg
    }

    pub fn get_sym_register(&mut self, sym: SymId, syms: &SymDefTable, types: &TypeTable) -> IlRegister {
        if let Some(reg) = self.func.reg_map.get_sym_reg(sym) {
            reg
        } else {
            let reg = self.allocate_register();

            log_writeln!(self.log, "Allocated {} for symbol {}", reg, sym);

            self.func.reg_map.add_sym_reg(sym, reg);
            self.func.reg_map.add_reg_info(reg, match *syms.find(sym) {
                SymDef { node: SymDefKind::Function(_), .. } => unimplemented!(),
                SymDef { node: SymDefKind::Local, ty, .. } => IlRegisterInfo(
                    IlRegisterType::Local(sym, IlSpanId::dummy()),
                    map_to_il_type(ty, types)
                ),
                SymDef { node: SymDefKind::Param(p), ty, .. } => IlRegisterInfo(
                    IlRegisterType::Param(sym, p),
                    map_to_il_type(ty, types)
                )
            });

            reg
        }
    }

    pub fn append_instruction(&mut self, i: IlInstructionKind, span: Span) {
        let span = if i.requires_unique_span_id() {
            self.func.spans.force_append(span, IlSpanId::dummy())
        } else {
            self.func.spans.append(span, IlSpanId::dummy())
        };
        self.current_block.instrs.push(IlInstruction::new(i, span));
    }

    pub fn end_block(&mut self) -> IlBlockId {
        self.func.append_block(mem::replace(&mut self.current_block, IlBlock::new()))
    }

    pub fn next_block_id(&self) -> IlBlockId {
        self.func.next_block_id
    }

    pub fn append_ending_instruction(&mut self, i: IlEndingInstructionKind, span: Span) -> IlBlockId {
        let span = self.func.spans.append(span, IlSpanId::dummy());
        self.current_block.end_instr = IlEndingInstruction::new(i, span);
        self.end_block()
    }

    pub fn finish(mut self) -> IlFunction {
        self.end_block();
        self.func
    }
}

struct IlGenContext<'a> {
    func: SymId,
    types: &'a TypeTable,
    syms: &'a SymDefTable,
    log: SharedLog<'a>
}

fn generate_direct_call_il<'a>(
    func: SymId,
    args: impl IntoIterator<Item=&'a ast::Expr>,
    span: Span,
    tgt: IlRegister,
    b: &mut IlBuilder,
    ctx: &mut IlGenContext
) -> IlRegister {
    let func_id = match ctx.syms.find(func).node {
        SymDefKind::Function(ref func_id) => func_id,
        _ => unreachable!()
    };

    // Some builtins require control over when their arguments are evaluated. For simplicity, we
    // handle them here, then evaluate arguments as normal and treat everything else like a normal
    // function call.
    match *func_id {
        FunctionId::AndBool => {
            let mut args = args.into_iter();

            let o1 = args.next().unwrap();
            let o1_r = generate_expression_il_to_temp(o1, b, ctx);
            let o1_end_block = b.append_ending_instruction(
                IlEndingInstructionKind::JumpZeroI1(IlBlockId::dummy(), IlOperand::Register(o1_r)),
                o1.span
            );

            let o2 = args.next().unwrap();
            let o2_r = generate_expression_il_to_temp(o2, b, ctx);
            let o2_end_block = b.append_ending_instruction(
                IlEndingInstructionKind::JumpZeroI1(IlBlockId::dummy(), IlOperand::Register(o2_r)),
                o2.span
            );

            b.append_instruction(
                IlInstructionKind::Copy(tgt, IlOperand::Const(IlConst::I1(true))),
                span
            );
            let true_block = b.append_ending_instruction(
                IlEndingInstructionKind::Jump(IlBlockId::dummy()),
                span
            );

            b.append_instruction(
                IlInstructionKind::Copy(tgt, IlOperand::Const(IlConst::I1(false))),
                span
            );
            let false_block = b.end_block();
            let end_block = b.end_block();

            b.relink_block_target(o1_end_block, false_block);
            b.relink_block_target(o2_end_block, false_block);
            b.relink_block_target(true_block, end_block);

            return tgt;
        },
        FunctionId::OrBool => {
            let mut args = args.into_iter();

            let o1 = args.next().unwrap();
            let o1_r = generate_expression_il_to_temp(o1, b, ctx);
            let o1_end_block = b.append_ending_instruction(
                IlEndingInstructionKind::JumpNonZeroI1(IlBlockId::dummy(), IlOperand::Register(o1_r)),
                o1.span
            );

            let o2 = args.next().unwrap();
            let o2_r = generate_expression_il_to_temp(o2, b, ctx);
            let o2_end_block = b.append_ending_instruction(
                IlEndingInstructionKind::JumpNonZeroI1(IlBlockId::dummy(), IlOperand::Register(o2_r)),
                o2.span
            );

            b.append_instruction(
                IlInstructionKind::Copy(tgt, IlOperand::Const(IlConst::I1(false))),
                span
            );
            let false_block = b.append_ending_instruction(
                IlEndingInstructionKind::Jump(IlBlockId::dummy()),
                span
            );

            b.append_instruction(
                IlInstructionKind::Copy(tgt, IlOperand::Const(IlConst::I1(true))),
                span
            );
            let true_block = b.end_block();
            let end_block = b.end_block();

            b.relink_block_target(o1_end_block, true_block);
            b.relink_block_target(o2_end_block, true_block);
            b.relink_block_target(false_block, end_block);

            return tgt;
        },
        _ => {}
    };

    let args: Vec<_> = args.into_iter()
        .map(|e| generate_expression_il_to_temp(e, b, ctx))
        .collect();

    match *func_id {
        FunctionId::UserDefined(_) | FunctionId::External(_) => {
            b.append_instruction(
                IlInstructionKind::Call(
                    tgt,
                    args.into_iter().map(IlOperand::Register).collect(),
                    func,
                    0
                ),
                span
            );
        },
        FunctionId::PrintI32 => {
            b.append_instruction(
                IlInstructionKind::PrintI32(IlOperand::Register(args[0])),
                span
            );
        },
        FunctionId::AddI32 => {
            b.append_instruction(
                IlInstructionKind::AddI32(
                    tgt,
                    IlOperand::Register(args[0]),
                    IlOperand::Register(args[1])
                ),
                span
            );
        },
        FunctionId::SubI32 => {
            b.append_instruction(
                IlInstructionKind::SubI32(
                    tgt,
                    IlOperand::Register(args[0]),
                    IlOperand::Register(args[1])
                ),
                span
            );
        },
        FunctionId::MulI32 => {
            b.append_instruction(
                IlInstructionKind::MulI32(
                    tgt,
                    IlOperand::Register(args[0]),
                    IlOperand::Register(args[1])
                ),
                span
            );
        },
        FunctionId::EqI32 => {
            b.append_instruction(
                IlInstructionKind::EqI32(
                    tgt,
                    IlOperand::Register(args[0]),
                    IlOperand::Register(args[1])
                ),
                span
            );
        },
        FunctionId::NeI32 => {
            b.append_instruction(
                IlInstructionKind::NeI32(
                    tgt,
                    IlOperand::Register(args[0]),
                    IlOperand::Register(args[1])
                ),
                span
            );
        },
        FunctionId::NegI32 => {
            b.append_instruction(
                IlInstructionKind::NegI32(tgt, IlOperand::Register(args[0])),
                span
            );
        },
        FunctionId::NotI32 => {
            b.append_instruction(
                IlInstructionKind::NotI32(tgt, IlOperand::Register(args[0])),
                span
            );
        },
        FunctionId::EqBool => {
            b.append_instruction(
                IlInstructionKind::EqI1(
                    tgt,
                    IlOperand::Register(args[0]),
                    IlOperand::Register(args[1])
                ),
                span
            );
        },
        FunctionId::NeBool => {
            b.append_instruction(
                IlInstructionKind::NeI1(
                    tgt,
                    IlOperand::Register(args[0]),
                    IlOperand::Register(args[1])
                ),
                span
            );
        },
        FunctionId::NotBool => {
            b.append_instruction(
                IlInstructionKind::NotI1(tgt, IlOperand::Register(args[0])),
                span
            );
        },
        FunctionId::AndBool | FunctionId::OrBool => unreachable!()
    };

    tgt
}

fn generate_expression_il(e: &ast::Expr, tgt: IlRegister, b: &mut IlBuilder, ctx: &mut IlGenContext) -> IlRegister {
    use ast::ExprKind::*;

    log_writeln!(ctx.log, "{} <= {} <- {}", b.next_block_id(), tgt, e.pretty(ctx.types, 2));

    match e.node {
        Call(box ref func, ref args) => {
            match *ctx.types.find_type(func.ty) {
                Type::FuncKnown(id, _) => {
                    generate_expression_il_to_temp(func, b, ctx);
                    generate_direct_call_il(
                        id,
                        args,
                        e.span,
                        tgt,
                        b,
                        ctx
                    );
                },
                Type::Func(_, _) => unimplemented!(),
                _ => unreachable!()
            }
        },
        BinOp(_, box (ref lhs, ref rhs), func) => {
            generate_direct_call_il(
                func.unwrap(),
                [lhs, rhs].iter().cloned(),
                e.span,
                tgt,
                b,
                ctx
            );
        },
        UnOp(_, box ref val, func) => {
            generate_direct_call_il(
                func.unwrap(),
                [val].iter().cloned(),
                e.span,
                tgt,
                b,
                ctx
            );
        },
        Id(ref id) => {
            if map_to_il_type(e.ty, ctx.types) != IlType::Void {
                let reg = b.get_sym_register(id.sym_id.unwrap(), ctx.syms, ctx.types);
                b.append_instruction(IlInstructionKind::Copy(tgt, IlOperand::Register(reg)), e.span);
            };
        },
        Block(box ref block) => {
            generate_block_il(block, tgt, b, ctx);
        },
        Tuple(ref vals) => {
            match vals.len() {
                0 => {},
                1 => {
                    generate_expression_il(&vals[0], tgt, b, ctx);
                },
                _ => unimplemented!()
            };
        },
        Paren(ref val) => {
            generate_expression_il(val, tgt, b, ctx);
        },
        If(box (ref cond, ref true_block, ref false_block)) => {
            let cond_r = generate_expression_il(
                cond,
                b.allocate_temp(IlType::I32),
                b,
                ctx
            );
            let cond_end_block = b.append_ending_instruction(
                IlEndingInstructionKind::JumpZeroI1(IlBlockId::dummy(), IlOperand::Register(cond_r)),
                cond.span
            );

            generate_block_il(true_block, tgt, b, ctx);
            let true_end_block = b.append_ending_instruction(
                IlEndingInstructionKind::Jump(IlBlockId::dummy()),
                true_block.span
            );

            let false_start_block = b.end_block();

            b.relink_block_target(cond_end_block, false_start_block);

            if let Some(false_block) = false_block {
                generate_block_il(false_block, tgt, b, ctx);
                b.end_block();
            };

            let end_block = b.end_block();

            b.relink_block_target(true_end_block, end_block);
        },
        While(box (ref cond, ref block)) => {
            let icond_r = generate_expression_il_to_temp(cond, b, ctx);
            let icond_block = b.append_ending_instruction(
                IlEndingInstructionKind::JumpZeroI1(IlBlockId::dummy(), IlOperand::Register(icond_r)),
                cond.span
            );

            let loop_block = b.next_block_id();
            generate_block_il(block, tgt, b, ctx);

            let cond_r = generate_expression_il_to_temp(cond, b, ctx);
            b.append_ending_instruction(
                IlEndingInstructionKind::JumpNonZeroI1(loop_block, IlOperand::Register(cond_r)),
                cond.span
            );

            b.relink_block_target(icond_block, b.next_block_id());
        },
        Lambda(ast::LambdaBody::Lifted(_)) => {
            // Intentionally generate no code, since lambdas are currently represented as IlType::Void
        },
        Lambda(ast::LambdaBody::Inline(_)) => unreachable!(),
        Int(val) => {
            match ctx.types.find_type(e.ty) {
                Type::I32 => b.append_instruction(
                    IlInstructionKind::Copy(tgt, IlOperand::Const(IlConst::I32(val as i32))),
                    e.span
                ),
                _ => unreachable!()
            };
        },
        Bool(val) => {
            b.append_instruction(
                IlInstructionKind::Copy(tgt, IlOperand::Const(IlConst::I1(val))),
                e.span
            );
        }
    };

    tgt
}

fn generate_expression_il_to_temp(e: &ast::Expr, b: &mut IlBuilder, ctx: &mut IlGenContext) -> IlRegister {
    generate_expression_il(
        e,
        b.allocate_temp(map_to_il_type(e.ty, ctx.types)),
        b,
        ctx
    )
}

fn generate_expression_pre_store(e: &ast::Expr, b: &mut IlBuilder, ctx: &mut IlGenContext) -> IlRegister {
    use ast::ExprKind::*;

    match e.node {
        Id(ref id) => {
            b.get_sym_register(id.sym_id.unwrap(), ctx.syms, ctx.types)
        },
        _ => unreachable!()
    }
}

fn generate_expression_store(e: &ast::Expr, reg: IlRegister, b: &mut IlBuilder, ctx: &mut IlGenContext) {
    use ast::ExprKind::*;

    log_writeln!(ctx.log, "{} <= {} -> {}", b.next_block_id(), reg, e.pretty(ctx.types, 2));

    if map_to_il_type(e.ty, ctx.types) == IlType::Void {
        return;
    };

    match e.node {
        Id(ref id) => {
            assert_eq!(reg, b.get_sym_register(id.sym_id.unwrap(), ctx.syms, ctx.types));
        },
        _ => unreachable!()
    };
}

fn generate_statement_il(s: &ast::Stmt, b: &mut IlBuilder, ctx: &mut IlGenContext) {
    use ast::StmtKind::*;

    log_writeln!(ctx.log, "{} <= {}", b.next_block_id(), s.pretty(ctx.types, 2));

    match s.node {
        Let(ref decl, ref val) => {
            let reg = b.get_sym_register(decl.id.sym_id.unwrap(), ctx.syms, ctx.types);
            generate_expression_il(val, reg, b, ctx);
        },
        Return(ref val) => {
            let reg = generate_expression_il_to_temp(val, b, ctx);
            b.append_ending_instruction(
                IlEndingInstructionKind::Return(IlOperand::Register(reg)),
                s.span
            );
        },
        Expr(ref val) => {
            generate_expression_il_to_temp(val, b, ctx);
        },
        Assign(ref rhs, ref lhs) => {
            let reg = generate_expression_pre_store(rhs, b, ctx);
            generate_expression_il(lhs, reg, b, ctx);
            generate_expression_store(rhs, reg, b, ctx);
        }
    }
}

fn generate_block_il(block: &ast::Block, tgt: IlRegister, b: &mut IlBuilder, ctx: &mut IlGenContext) -> IlRegister {
    for s in block.stmts.iter() {
        generate_statement_il(s, b, ctx);
    };

    if let Some(ref result) = block.result {
        generate_expression_il(result, tgt, b, ctx);
    };

    tgt
}

fn generate_function_il(f: &ast::Function, ctx: &mut IlGenContext) -> IlFunction {
    let sym = f.sym_id.unwrap();
    let mut b = IlBuilder::new(sym, ctx.log.clone());

    ctx.func = sym;

    log_writeln!(ctx.log, "===== GENERATING IL FOR FUNCTION {} =====\n", ctx.func);

    let param_regs = f.sig.params.iter().map(|decl| {
        b.get_sym_register(decl.id.sym_id.unwrap(), ctx.syms, ctx.types)
    }).collect_vec();
    b.func.reg_map.set_params(param_regs);

    let result_reg = generate_block_il(
        &f.body,
        b.allocate_temp(
            f.body.result.as_ref().map_or_else(|| IlType::Void, |r| map_to_il_type(r.ty, ctx.types))
        ),
        &mut b,
        ctx
    );

    log_writeln!(ctx.log, "{} <= implicit return of {}", b.next_block_id(), result_reg);
    b.append_ending_instruction(
        IlEndingInstructionKind::Return(IlOperand::Register(result_reg)),
        f.body.result.as_ref().map_or_else(Span::dummy, |r| r.span)
    );

    let f = b.finish();

    log_writeln!(ctx.log, "\n===== GENERATED IL =====\n\n{}\n{}", f, f.spans);

    f
}

pub fn generate_il(m: &ast::Module, log: &mut Log) -> IlProgram {
    let mut ctx = IlGenContext {
        func: SymId(0),
        types: &m.types,
        syms: &m.syms,
        log: log.share()
    };
    let mut program = IlProgram::new();

    log_writeln!(ctx.log, "===== IL GENERATION =====\n");

    for f in m.funcs.iter() {
        program.funcs.insert(
            f.sym_id.unwrap(),
            generate_function_il(f, &mut ctx)
        );
    };

    program
}
