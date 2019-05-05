use std::mem;

use itertools::Itertools;

use ::ast::*;
use ::sym::{self, ScopedSymRefTable, SymId};
use ::types::{Type, TypeId};

use super::{AnalysisContext};
use super::error::{Error, ErrorKind};

fn get_possible_call_signatures<'a, T>(
    arg_types: &[TypeId],
    sigs: impl IntoIterator<Item=(T, &'a [TypeId])>,
    ctx: &AnalysisContext
) -> Vec<T> {
    sigs.into_iter().filter_map(|(val, param_types)| {
        if param_types.len() != arg_types.len() {
            return None;
        };

        let is_valid = param_types.iter().zip(arg_types).all(|(&ptype, &atype)| {
            atype.is_unknown() || ptype.is_unknown() || ctx.types.can_trivially_convert(atype, ptype)
        });

        if is_valid {
            Some(val)
        } else {
            None
        }
    }).collect()
}

fn deduce_type_internal(e: &mut Expr, expected_ty: TypeId, ctx: &mut AnalysisContext) -> TypeId {
    match e.node {
        ExprKind::Call(ref mut func, ref mut args) => {
            let func_type = deduce_type(func, TypeId::unknown(), ctx);
            let arg_types: Vec<_> = args.iter_mut().map(|a| deduce_type(a, TypeId::unknown(), ctx)).collect();

            if arg_types.iter().any(|ty| ty.is_error()) {
                return TypeId::for_error();
            };

            let (ret_type, mut param_types) = match *ctx.types.find_type(func_type) {
                Type::Error => return TypeId::for_error(),
                Type::Func(ret_type, ref param_types) => (ret_type, param_types.clone()),
                Type::FuncKnown(_, sig_id) => match *ctx.types.find_type(sig_id) {
                    Type::Func(ret_type, ref param_types) => (ret_type, param_types.clone()),
                    _ => unreachable!()
                },
                Type::Undecided(ref types) => {
                    let fn_types: Vec<_> = types.iter().filter_map(|&ty| {
                        ctx.types.get_call_signature(ty).map(|(ret, params)| (ret, params))
                    }).collect();

                    if fn_types.is_empty() {
                        cannot_call!(e, func, ctx);
                        return TypeId::for_error();
                    };

                    let possible_sigs = get_possible_call_signatures(
                        arg_types.as_ref(),
                        fn_types.iter().map(|&(ret, ref params)| ((params, ret), params.as_ref())),
                        ctx
                    );

                    match &possible_sigs[..] {
                        &[] => {
                            invalid_call_signature!(e, func, arg_types.clone(), ctx);
                            return TypeId::for_error();
                        },
                        &[(param_tys, ret_ty)] => {
                            (ret_ty, param_tys.clone())
                        },
                        sigs => return ctx.types.get_or_add_undecided_type(sigs.iter().map(|&(_, t)| t).collect())
                    }
                },
                _ => {
                    cannot_call!(e, func, ctx);
                    return TypeId::for_error();
                }
            };

            for (arg, param_ty) in args.iter_mut().zip(&mut param_types) {
                if param_ty.is_unknown() {
                    *param_ty = deduce_type(arg, TypeId::unknown(), ctx);
                };
            };

            let func_type = ctx.types.get_or_add_function_type(ret_type, param_types);
            let func_type = deduce_type(func, func_type, ctx);

            let (ret_type, param_types) = ctx.types.get_call_signature(func_type).unwrap();

            for (arg, &param_ty) in args.iter_mut().zip(&param_types) {
                deduce_type(arg, param_ty, ctx);
            };

            if expect_resolved_type!(func, ctx) {
                if args.len() != param_types.len() || args.iter().zip(param_types).any(|(arg, param_ty)| !ctx.types.can_trivially_convert(arg.ty, param_ty)) {
                    invalid_call_signature!(e, func, args.iter().map(|arg| arg.ty).collect(), ctx);
                    if ret_type.is_unknown() {
                        return TypeId::for_error();
                    };
                };

                ret_type
            } else {
                TypeId::for_error()
            }
        },
        ExprKind::BinOp(op, box (ref mut lhs, ref mut rhs), ref mut rfunc) => {
            let val_types = [
                deduce_type(lhs, TypeId::unknown(), ctx),
                deduce_type(rhs, TypeId::unknown(), ctx)
            ];

            if val_types[0].is_error() || val_types[1].is_error() {
                return TypeId::for_error();
            };

            let op_builtins = sym::BUILTIN_BINARY_OPS.get(&op).map_or_else(|| [].as_ref(), |impls| impls.as_ref());

            // TODO Allow operator overloading
            let possible_sigs = get_possible_call_signatures(
                val_types.as_ref(),
                op_builtins.iter().map(|&(ref f, ref params, ret)| ((f, params, ret), params.as_ref())),
                ctx
            );

            match &possible_sigs[..] {
                &[] => {
                    invalid_bin_op_type!(e, op, lhs, rhs, ctx);
                    TypeId::for_error()
                },
                &[(f, param_tys, ty)] => {
                    deduce_type(lhs, param_tys[0], ctx);
                    deduce_type(rhs, param_tys[1], ctx);

                    expect_type!(lhs, param_tys[0], ctx);
                    expect_type!(rhs, param_tys[1], ctx);

                    *rfunc = Some(ctx.sym_defs.find_builtin(f, ctx.types));

                    ty
                },
                sigs => ctx.types.get_or_add_undecided_type(sigs.iter().map(|&(_, _, t)| t).collect())
            }
        },
        ExprKind::UnOp(op, box ref mut val, ref mut rfunc) => {
            let val_types = [deduce_type(val, TypeId::unknown(), ctx)];

            if val_types[0].is_error() {
                return TypeId::for_error();
            };

            let op_builtins = sym::BUILTIN_UNARY_OPS.get(&op).map_or_else(|| [].as_ref(), |impls| impls.as_ref());

            // TODO Allow operator overloading
            let possible_sigs = get_possible_call_signatures(
                val_types.as_ref(),
                op_builtins.iter().map(|&(ref f, ref params, ret)| ((f, params, ret), params.as_ref())),
                ctx
            );

            match &possible_sigs[..] {
                &[] => {
                    invalid_un_op_type!(e, op, val, ctx);
                    TypeId::for_error()
                },
                &[(f, param_tys, ty)] => {
                    assert_eq!(deduce_type(val, param_tys[0], ctx), param_tys[0]);

                    *rfunc = Some(ctx.sym_defs.find_builtin(f, ctx.types));

                    ty
                },
                sigs => ctx.types.get_or_add_undecided_type(sigs.iter().map(|&(_, _, t)| t).collect())
            }
        },
        ExprKind::Id(ref mut id) => {
            if let Some(sym_id) = id.sym_id {
                let sym = ctx.sym_defs.find(sym_id);

                e.assignable = if sym.mutable {
                    Assignability::Assignable
                } else {
                    Assignability::Immutable(sym_id)
                };
                sym.ty
            } else {
                e.assignable = Assignability::Assignable;
                TypeId::for_error()
            }
        },
        ExprKind::Block(box ref mut block) => deduce_block_type(block, expected_ty, ctx),
        ExprKind::Tuple(ref mut vals) => {
            let val_types: Vec<_> = vals.iter_mut().map(|v| deduce_type(v, TypeId::unknown(), ctx)).collect();

            ctx.types.get_or_add_tuple_type(val_types)
        },
        ExprKind::Paren(ref mut val) => deduce_type(val, expected_ty, ctx),
        ExprKind::If(box (ref mut cond, ref mut true_block, ref mut false_block)) => {
            if cond.ty.is_unknown() {
                deduce_type(cond, TypeId::for_bool(), ctx);
                expect_type!(cond, TypeId::for_bool(), ctx);
            };

            let true_type = deduce_block_type(true_block, expected_ty, ctx);

            if let Some(ref mut false_block) = *false_block {
                let false_type = deduce_block_type(false_block, expected_ty, ctx);

                if let Some(t) = ctx.types.least_upper_bound(true_type, false_type) {
                    assert_eq!(deduce_block_type(true_block, t, ctx), t);
                    assert_eq!(deduce_block_type(false_block, t, ctx), t);
                    t
                } else {
                    cannot_convert!(false_block.span, true_type, false_type, ctx);
                    TypeId::for_error()
                }
            } else {
                true_type
            }
        },
        ExprKind::While(box (ref mut cond, ref mut block)) => {
            if cond.ty.is_unknown() {
                deduce_type(cond, TypeId::for_bool(), ctx);
                expect_type!(cond, TypeId::for_bool(), ctx);
            };

            deduce_block_type(block, TypeId::for_empty_tuple(), ctx);

            if let Some(ref result) = block.result {
                expect_type!(result, TypeId::for_empty_tuple(), ctx);
            };

            TypeId::for_empty_tuple()
        },
        ExprKind::Lambda(LambdaBody::Inline(box ref mut func)) => {
            let mut ty = ctx.sym_defs.find(func.sym_id.unwrap()).ty;

            match *ctx.types.find_type(ty) {
                Type::FuncKnown(_, _) => {
                    analyze_function(func, ctx);
                    ty = ctx.sym_defs.find(func.sym_id.unwrap()).ty;
                },
                Type::Func(ret_type, ref param_types) => {
                    if param_types.iter().all(|&pt| !ctx.types.is_type_undecided(pt)) {
                        analyze_function(func, ctx);
                        ty = ctx.sym_defs.find(func.sym_id.unwrap()).ty;
                    } else if !expected_ty.is_unknown() {
                        if let Type::Func(_, ref actual_param_types) = ctx.types.find_type(expected_ty) {
                            if param_types.len() == actual_param_types.len() {
                                let param_types = param_types.clone();
                                let actual_param_types = actual_param_types.clone();

                                let param_types = param_types.into_iter().zip(actual_param_types.into_iter()).map(|(e, a)| ctx.types.least_upper_bound(e, a).unwrap_or(TypeId::unknown())).collect_vec();

                                let done = param_types.iter().all(|&pt| !ctx.types.is_type_undecided(pt));

                                for (&ty, param) in param_types.iter().zip(func.sig.params.iter_mut()) {
                                    param.ty.type_id = ty;
                                };

                                ty = ctx.sym_defs.narrow_known_function_type(
                                    func.sym_id.unwrap(),
                                    ctx.types.get_or_add_function_type(ret_type, param_types),
                                    ctx.types
                                );

                                if done {
                                    analyze_function(func, ctx);
                                    ty = ctx.sym_defs.find(func.sym_id.unwrap()).ty;
                                };
                            };
                        };
                    };
                },
                _ => unreachable!()
            };
            ty
        },
        ExprKind::Lambda(LambdaBody::Lifted(_)) => unreachable!(),
        ExprKind::Int(_) => {
            // TODO Implement multiple int types
            TypeId::for_i32()
        },
        ExprKind::Bool(_) => TypeId::for_bool()
    }
}

pub fn deduce_type(e: &mut Expr, expected_ty: TypeId, ctx: &mut AnalysisContext) -> TypeId {
    if !ctx.types.is_type_undecided(e.ty) || (expected_ty.is_unknown() && !e.ty.is_unknown()) {
        return e.ty;
    };

    let ty = deduce_type_internal(e, expected_ty, ctx);

    e.ty = ty;
    ty
}

pub fn analyze_statement(s: &mut Stmt, ctx: &mut AnalysisContext) {
    match s.node {
        StmtKind::Let(ref mut decl, ref mut val) => {
            let sym_id = decl.id.sym_id.unwrap();
            let ty = ctx.sym_defs.find(sym_id).ty;

            if ty.is_unknown() {
                deduce_type(val, TypeId::unknown(), ctx);
                let ty = if expect_resolved_type!(val, ctx) {
                    val.ty
                } else {
                    TypeId::for_error()
                };

                ctx.sym_defs.find_mut(sym_id).ty = ty;
            } else {
                deduce_type(val, ty, ctx);
                expect_type!(val, ty, ctx);
            };
        },
        StmtKind::Return(_) => unimplemented!(),
        StmtKind::Assign(ref mut lhs, ref mut rhs) => {
            let ty = deduce_type(lhs, TypeId::unknown(), ctx);

            match lhs.assignable {
                Assignability::Assignable => {},
                Assignability::NotAssignable => cannot_assign!(lhs, ctx),
                Assignability::Immutable(sym_id) => {
                    let sym = ctx.sym_defs.find(sym_id);
                    cannot_assign_immutable!(lhs, sym.name.clone(), ctx);
                }
            };

            deduce_type(rhs, ty, ctx);
            expect_type!(rhs, ty, ctx);
        },
        StmtKind::Expr(ref mut expr) => {
            deduce_type(expr, TypeId::unknown(), ctx);
            expect_resolved_type!(expr, ctx);
        }
    }
}

pub fn deduce_block_type(b: &mut Block, expected_ty: TypeId, ctx: &mut AnalysisContext) -> TypeId {
    for s in b.stmts.iter_mut() {
        analyze_statement(s, ctx);
    };

    let ty = b.result.as_mut().map_or(TypeId::for_empty_tuple(), |val| deduce_type(val, expected_ty, ctx));
    ty
}

pub fn analyze_function(f: &mut Function, ctx: &mut AnalysisContext) {
    let old_fn_sym = mem::replace(&mut ctx.fn_sym, f.sym_id.unwrap());

    let ty = deduce_block_type(&mut f.body, f.sig.return_type.type_id, ctx);

    if f.sig.return_type.is_infer() {
        f.sig.return_type.type_id = ty;
    };

    let expected_ty = f.sig.return_type.type_id;

    if let Some(ref result) = f.body.result {
        expect_type!(result, f.sig.return_type.type_id, ctx);
    } else if expected_ty != TypeId::for_error() && expected_ty != TypeId::for_empty_tuple() {
        ctx.errors.push(Error(
            ErrorKind::CannotConvert { actual: TypeId::for_empty_tuple(), expected: expected_ty },
            f.body.span
        ));
    };

    ctx.sym_defs.narrow_known_function_type(
        f.sym_id.unwrap(),
        ctx.types.get_or_add_function_type(
            f.sig.return_type.type_id,
            f.sig.params.iter().map(|decl| decl.ty.type_id).collect()
        ),
        ctx.types
    );

    ctx.fn_sym = old_fn_sym;
}


pub fn analyze_module(m: &mut Module, errors: &mut Vec<Error>) {
    let ctx = &mut AnalysisContext {
        types: &mut m.types,
        sym_defs: &mut m.syms,
        sym_refs: ScopedSymRefTable::new(),
        errors,
        fn_sym: SymId(!0)
    };

    for f in m.funcs.iter_mut() {
        analyze_function(f, ctx);
    };
}
