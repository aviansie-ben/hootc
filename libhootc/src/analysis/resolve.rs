use std::mem;

use super::AnalysisContext;
use super::error::{Error, ErrorKind};

use crate::ast::*;
use crate::sym::{FunctionId, ScopedSymRefTable, SymDef, SymId};
use crate::types::TypeId;

fn find_type_internal(ty: &mut Ty, ctx: &mut AnalysisContext) -> TypeId {
    match ty.node {
        TyKind::Ident(ref name) => if let Some(ty) = ctx.sym_refs.find_type(name) {
            ty
        } else {
            ctx.errors.push(Error(ErrorKind::UndeclaredType(name.to_owned()), ty.span));
            TypeId::for_error()
        },
        TyKind::Tuple(ref mut types) => {
            let types: Vec<_> = types.iter_mut().map(|ty| find_type(ty, ctx)).collect();
            ctx.types.get_or_add_tuple_type(types)
        },
        TyKind::Ref(_) => unimplemented!(),
        TyKind::Infer => TypeId::unknown()
    }
}

fn find_type(ty: &mut Ty, ctx: &mut AnalysisContext) -> TypeId {
    if !ty.type_id.is_unknown() {
        return ty.type_id;
    };

    let type_id = find_type_internal(ty, ctx);

    ty.type_id = type_id;
    type_id
}

fn resolve_expr(e: &mut Expr, ctx: &mut AnalysisContext) {
    match e.node {
        ExprKind::Call(ref mut func, ref mut args) => {
            resolve_expr(func, ctx);
            for a in args.iter_mut() {
                resolve_expr(a, ctx);
            };
        },
        ExprKind::BinOp(_, box (ref mut lhs, ref mut rhs), _) => {
            resolve_expr(lhs, ctx);
            resolve_expr(rhs, ctx);
        },
        ExprKind::UnOp(_, box ref mut val, _) => {
            resolve_expr(val, ctx);
        },
        ExprKind::Id(ref mut id) => {
            if let Some(sym_id) = ctx.sym_refs.find(&id.id) {
                let sym = ctx.sym_defs.find(sym_id);
                if sym.fn_sym.is_some() && sym.fn_sym != Some(ctx.fn_sym) {
                    lambda_capture_not_supported!(e, ctx);
                };

                id.sym_id = Some(sym_id);
            } else {
                ctx.errors.push(Error(ErrorKind::UndeclaredIdentifier(id.id.clone()), e.span));
            };
        },
        ExprKind::Block(box ref mut block) => {
            resolve_block(block, ctx);
        },
        ExprKind::Tuple(ref mut vals) => {
            for v in vals.iter_mut() {
                resolve_expr(v, ctx);
            };
        },
        ExprKind::Paren(ref mut val) => {
            resolve_expr(val, ctx);
        },
        ExprKind::If(box (ref mut cond, ref mut true_block, ref mut false_block)) => {
            resolve_expr(cond, ctx);
            resolve_block(true_block, ctx);
            if let Some(ref mut false_block) = *false_block {
                resolve_block(false_block, ctx);
            };
        },
        ExprKind::While(box (ref mut cond, ref mut do_block)) => {
            resolve_expr(cond, ctx);
            resolve_block(do_block, ctx);
        },
        ExprKind::Lambda(LambdaBody::Inline(box ref mut func)) => {
            resolve_function_decl(func, false, ctx);
            resolve_function(func, ctx);
        },
        ExprKind::Lambda(LambdaBody::Lifted(_)) => unreachable!(),
        ExprKind::Int(_) => {},
        ExprKind::Bool(_) => {}
    };
}

fn resolve_stmt(s: &mut Stmt, ctx: &mut AnalysisContext) {
    match s.node {
        StmtKind::Let(ref mut decl, ref mut val) => {
            resolve_expr(val, ctx);

            let ty = find_type(&mut decl.ty, ctx);
            let sym_id = ctx.sym_defs.add_symbol(SymDef::local(
                &decl.id,
                ty,
                ctx.fn_sym,
                decl.mutable
            ));

            ctx.sym_refs.top_mut().add(decl.id.id.clone(), sym_id);
            decl.id.sym_id = Some(sym_id);
        },
        StmtKind::Return(ref mut val) => {
            resolve_expr(val, ctx);
        },
        StmtKind::Assign(ref mut lhs, ref mut rhs) => {
            resolve_expr(lhs, ctx);
            resolve_expr(rhs, ctx);
        },
        StmtKind::Expr(ref mut expr) => {
            resolve_expr(expr, ctx);
        }
    }
}

fn resolve_block(b: &mut Block, ctx: &mut AnalysisContext) {
    ctx.sym_refs.push_scope();

    for s in b.stmts.iter_mut() {
        resolve_stmt(s, ctx);
    };

    if let Some(ref mut result) = b.result {
        resolve_expr(result, ctx);
    };

    ctx.sym_refs.pop_scope();
}

fn resolve_function(f: &mut Function, ctx: &mut AnalysisContext) {
    let old_fn_sym = mem::replace(&mut ctx.fn_sym, f.sym_id.unwrap());
    ctx.sym_refs.push_scope();

    for (i, ref mut decl) in f.sig.params.iter_mut().enumerate() {
        let sym_id = ctx.sym_defs.add_symbol(
            SymDef::param(&decl.id, decl.ty.type_id, i as u32, ctx.fn_sym, decl.mutable)
        );

        ctx.sym_refs.top_mut().add(decl.id.id.clone(), sym_id);
        decl.id.sym_id = Some(sym_id);
    };

    resolve_block(&mut f.body, ctx);

    ctx.sym_refs.pop_scope();
    ctx.fn_sym = old_fn_sym;
}

fn resolve_function_decl(f: &mut Function, add_ref: bool, ctx: &mut AnalysisContext) {
    for decl in f.sig.params.iter_mut() {
        find_type(&mut decl.ty, ctx);
    };

    find_type(&mut f.sig.return_type, ctx);

    let sym_id = ctx.sym_defs.add_known_function_symbol(SymDef::func(
        &f.name,
        ctx.types.get_or_add_function_type(
            f.sig.return_type.type_id,
            f.sig.params.iter().map(|decl| decl.ty.type_id).collect()
        ),
        FunctionId::UserDefined(SymId(!0)),
        None
    ), ctx.types);

    if add_ref {
        ctx.sym_refs.top_mut().add(f.name.id.clone(), sym_id);
    };

    f.sym_id = Some(sym_id);
}

fn resolve_import(i: &mut Import, ctx: &mut AnalysisContext) {
    // TODO Implement for reals
    if let Some(ref path) = i.path {
        if path.parts.len() == 1 && path.parts[0].id == "std" {
            for part in i.parts.iter_mut() {
                let sym_id = match part.id.id.as_ref() {
                    "print_i32" => ctx.sym_defs.find_builtin(&FunctionId::PrintI32, ctx.types),
                    _ => unimplemented!()
                };
                ctx.sym_refs.top_mut().add(part.as_id.as_ref().unwrap_or(&part.id).id.clone(), sym_id);
            };
        } else {
            unimplemented!();
        };
    } else {
        unimplemented!();
    };
}

fn add_builtins(ctx: &mut AnalysisContext) {
    ctx.sym_refs.top_mut().add_type("i32".to_owned(), TypeId::for_i32());
    ctx.sym_refs.top_mut().add_type("bool".to_owned(), TypeId::for_bool());
}

pub fn resolve_module(m: &mut Module, errors: &mut Vec<Error>) {
    let ctx = &mut AnalysisContext {
        types: &mut m.types,
        sym_defs: &mut m.syms,
        sym_refs: ScopedSymRefTable::new(),
        errors,
        fn_sym: SymId(!0)
    };

    ctx.sym_refs.push_scope();
    add_builtins(ctx);

    for i in m.imports.iter_mut() {
        resolve_import(i, ctx);
    };

    for f in m.funcs.iter_mut() {
        resolve_function_decl(f, true, ctx);
    };

    for f in m.funcs.iter_mut() {
        resolve_function(f, ctx);
    };

    ctx.sym_refs.pop_scope();
}
