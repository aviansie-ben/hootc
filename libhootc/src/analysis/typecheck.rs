use ::ast::{Block, Expr, ExprKind, Function, Import, Module, Stmt, StmtKind, Ty, TyKind};
use ::sym::{self, FunctionId, ScopedSymRefTable, SymDef};
use ::types::{Type, TypeId};

use super::{AnalysisContext};
use super::error::{Error, ErrorKind};
use ast::Assignability;

macro_rules! unused {
    ($var:ident) => {{
        let _ = $var;
    }};
    ($var:ident, $($rest:ident),*) => {{
        unused!($var);
        unused!($($rest),*);
    }};
}

macro_rules! expect_type {
    ($expr:expr, $etype:expr, $ctx:ident) => {{
        if $expr.ty != $etype && !$expr.ty.is_error() {
            cannot_convert!($expr.span, $etype, $expr.ty, $ctx);
            false
        } else {
            true
        }
    }}
}

macro_rules! cannot_convert {
    ($span:expr, $etype:expr, $atype:expr, $ctx:ident) => {{
        $ctx.errors.push(Error(
            ErrorKind::CannotConvert { expected: $etype, actual: $atype },
            $span
        ));
    }}
}

macro_rules! cannot_call {
    ($expr:expr, $func:expr, $ctx:ident) => {{
        $ctx.errors.push(Error(
            ErrorKind::CannotCallType($func.ty),
            $expr.span
        ))
    }}
}

macro_rules! invalid_call_signature {
    ($expr:expr, $func:expr, $arg_types:expr, $ctx:ident) => {{
        $ctx.errors.push(Error(
            ErrorKind::InvalidCallSignature { func_type: $func.ty, arg_types: $arg_types },
            $expr.span
        ))
    }}
}

macro_rules! invalid_bin_op_type {
    ($expr:expr, $op:expr, $lhs:expr, $rhs:expr, $ctx:ident) => {{
        $ctx.errors.push(Error(
            ErrorKind::NoSuchBinOp { op: $op, lhs_type: $lhs.ty, rhs_type: $rhs.ty },
            $expr.span
        ))
    }}
}

macro_rules! invalid_un_op_type {
    ($expr:expr, $op:expr, $val:expr, $ctx:ident) => {{
        $ctx.errors.push(Error(
            ErrorKind::NoSuchUnOp { op: $op, val_type: $val.ty },
            $expr.span
        ))
    }}
}

macro_rules! cannot_assign {
    ($expr:expr, $ctx:ident) => {{
        $ctx.errors.push(Error(
            ErrorKind::CannotAssignExpr,
            $expr.span
        ))
    }}
}

macro_rules! cannot_assign_immutable {
    ($expr:expr, $id:expr, $ctx:ident) => {{
        $ctx.errors.push(Error(
            ErrorKind::CannotAssignImmutable($id),
            $expr.span
        ))
    }}
}

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
            ctx.types.can_trivially_convert(atype, ptype)
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

            let (func_type, ret_type, param_types) = match *ctx.types.find_type(func_type) {
                Type::Error => return TypeId::for_error(),
                Type::Func(ret_type, ref param_types) => (func_type, ret_type, param_types.clone()),
                Type::Undecided(ref types) => {
                    let fn_types: Vec<_> = types.iter().filter_map(|&ty| {
                        ctx.types.get_call_signature(ty).map(|(ret, params)| (ty, ret, params))
                    }).collect();

                    if fn_types.is_empty() {
                        cannot_call!(e, func, ctx);
                        return TypeId::for_error();
                    };

                    let possible_sigs = get_possible_call_signatures(
                        arg_types.as_ref(),
                        fn_types.iter().map(|&(ty, ret, ref params)| ((ty, params, ret), params.as_ref())),
                        ctx
                    );

                    match &possible_sigs[..] {
                        &[] => {
                            invalid_call_signature!(e, func, arg_types.clone(), ctx);
                            return TypeId::for_error();
                        },
                        &[(ty, param_tys, ret_ty)] => {
                            (ty, ret_ty, param_tys.clone())
                        },
                        sigs => return ctx.types.get_or_add_undecided_type(sigs.iter().map(|&(_, _, t)| t).collect())
                    }
                },
                _ => {
                    cannot_call!(e, func, ctx);
                    return TypeId::for_error();
                }
            };

            assert_eq!(deduce_type(func, func_type, ctx), func_type);

            for (arg, &param_ty) in args.iter_mut().zip(&param_types) {
                deduce_type(arg, param_ty, ctx);
            };

            if args.iter().zip(param_types).any(|(arg, param_ty)| !ctx.types.can_trivially_convert(arg.ty, param_ty)) {
                invalid_call_signature!(e, func, args.iter().map(|arg| arg.ty).collect(), ctx);
            };

            ret_type
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
                    assert_eq!(deduce_type(lhs, param_tys[0], ctx), param_tys[0]);
                    assert_eq!(deduce_type(rhs, param_tys[1], ctx), param_tys[1]);

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
            if let Some(sym_id) = ctx.sym_refs.find(&id.id) {
                let sym = ctx.sym_defs.find(sym_id);

                e.assignable = if sym.mutable {
                    Assignability::Assignable
                } else {
                    Assignability::Immutable(sym_id)
                };

                id.sym_id = Some(sym_id);
                sym.ty
            } else {
                e.assignable = Assignability::Assignable;
                ctx.errors.push(Error(ErrorKind::UndeclaredIdentifier(id.id.clone()), e.span));
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
        ExprKind::Lambda(box (ref mut sig, ref mut block)) => {
            // TODO Implement this
            unused!(sig, block);
            unimplemented!()
        },
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

fn find_type_internal(ty: &mut Ty, ctx: &mut AnalysisContext) -> TypeId {
    match ty.node {
        TyKind::Ident(ref name) => match name.as_ref() {
            "i32" => TypeId::for_i32(),
            "bool" => TypeId::for_bool(),
            _ => unimplemented!()
        },
        TyKind::Tuple(ref mut types) => {
            let types: Vec<_> = types.iter_mut().map(|ty| find_type(ty, ctx)).collect();
            ctx.types.get_or_add_tuple_type(types)
        },
        TyKind::Ref(_) => unimplemented!(),
        TyKind::Infer => unreachable!()
    }
}

pub fn find_type(ty: &mut Ty, ctx: &mut AnalysisContext) -> TypeId {
    if !ty.type_id.is_unknown() {
        return ty.type_id;
    };

    let type_id = find_type_internal(ty, ctx);

    ty.type_id = type_id;
    type_id
}

pub fn analyze_statement(s: &mut Stmt, ctx: &mut AnalysisContext) {
    match s.node {
        StmtKind::Let(ref mut decl, ref mut val) => {
            if decl.id.sym_id.is_some() {
                return;
            };

            let ty = if decl.ty.is_infer() {
                deduce_type(val, TypeId::unknown(), ctx);
                val.ty
            } else {
                let ty = find_type(&mut decl.ty, ctx);

                deduce_type(val, ty, ctx);
                expect_type!(val, ty, ctx);

                ty
            };

            let sym_id = ctx.sym_defs.add_symbol(SymDef::local(&decl.id, ty, ctx.fn_id, decl.mutable));

            ctx.sym_refs.top_mut().add(decl.id.id.clone(), sym_id);
            decl.id.sym_id = Some(sym_id);
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
        }
    }
}

pub fn deduce_block_type(b: &mut Block, expected_ty: TypeId, ctx: &mut AnalysisContext) -> TypeId {
    ctx.sym_refs.push_scope();

    for s in b.stmts.iter_mut() {
        analyze_statement(s, ctx);
    };

    let ty = b.result.as_mut().map_or(TypeId::for_empty_tuple(), |val| deduce_type(val, expected_ty, ctx));

    ctx.sym_refs.pop_scope();
    ty
}

pub fn analyze_function(f: &mut Function, ctx: &mut AnalysisContext) {
    let old_fn_id = ctx.fn_id;

    ctx.fn_id = f.id;

    ctx.sym_refs.push_scope();

    for (i, ref mut decl) in f.sig.params.iter_mut().enumerate() {
        let sym_id = ctx.sym_defs.add_symbol(
            SymDef::param(&decl.id, decl.ty.type_id, i as u32, ctx.fn_id, decl.mutable)
        );

        ctx.sym_refs.top_mut().add(decl.id.id.clone(), sym_id);
        decl.id.sym_id = Some(sym_id);
    };

    deduce_block_type(&mut f.body, find_type(&mut f.sig.return_type, ctx), ctx);

    ctx.sym_refs.pop_scope();
    ctx.fn_id = old_fn_id;
}

pub fn analyze_import(i: &mut Import, ctx: &mut AnalysisContext) {
    // TODO Implement for reals
    if let Some(ref path) = i.path {
        if path.parts.len() == 1 && path.parts[0].id == "std" {
            for part in i.parts.iter_mut() {
                let sym_def = match part.id.id.as_ref() {
                    "print_i32" => SymDef::func(
                        &part.id,
                        ctx.types.get_or_add_function_type(
                            TypeId::for_empty_tuple(),
                            vec![TypeId::for_i32()]
                        ),
                        FunctionId::PrintI32,
                        None
                    ),
                    _ => unimplemented!()
                };
                let sym_id = ctx.sym_defs.add_symbol(sym_def);
                ctx.sym_refs.top_mut().add(part.as_id.as_ref().unwrap_or(&part.id).id.clone(), sym_id);
            };
        } else {
            unimplemented!();
        }
    } else {
        unimplemented!();
    };
}

pub fn analyze_module(m: &mut Module, errors: &mut Vec<Error>) {
    let mut ctx = AnalysisContext {
        types: &mut m.types,
        sym_defs: &mut m.syms,
        sym_refs: ScopedSymRefTable::new(),
        errors,
        fn_id: !0,
        next_fn_id: 0
    };

    ctx.sym_refs.push_scope();

    for i in m.imports.iter_mut() {
        analyze_import(i, &mut ctx);
    };

    for f in m.funcs.iter_mut() {
        f.id = ctx.next_fn_id;
        ctx.next_fn_id += 1;

        for ref mut decl in f.sig.params.iter_mut() {
            find_type(&mut decl.ty, &mut ctx);
        };

        find_type(&mut f.sig.return_type, &mut ctx);

        let sym_id = ctx.sym_defs.add_symbol(SymDef::func(
            &f.name,
            ctx.types.get_or_add_function_type(
                f.sig.return_type.type_id,
                f.sig.params.iter().map(|decl| decl.ty.type_id).collect()
            ),
            FunctionId::UserDefined(f.id),
            None
        ));
        ctx.sym_refs.top_mut().add(f.name.id.clone(), sym_id);

        f.sym_id = Some(sym_id);
    };

    for f in m.funcs.iter_mut() {
        analyze_function(f, &mut ctx);
    };

    ctx.sym_refs.pop_scope();
}
