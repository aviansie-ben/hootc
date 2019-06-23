use std::mem;

use ::ast::*;
use ::sym::{ScopedSymRefTable, SymId};

use super::{AnalysisContext};
use super::error::Error;

fn lift_from_expr(expr: &mut Expr, ctx: &mut AnalysisContext, lifted: &mut Vec<Function>) {
    match expr.node {
        ExprKind::Call(box ref mut func, ref mut args) => {
            lift_from_expr(func, ctx, lifted);
            for arg in args.iter_mut() {
                lift_from_expr(arg, ctx, lifted);
            };
        },
        ExprKind::BinOp(_, box (ref mut lhs, ref mut rhs), _) => {
            lift_from_expr(lhs, ctx, lifted);
            lift_from_expr(rhs, ctx, lifted);
        },
        ExprKind::UnOp(_, box ref mut val, _) => {
            lift_from_expr(val, ctx, lifted);
        },
        ExprKind::Id(_) => {},
        ExprKind::Block(box ref mut block) => {
            lift_from_block(block, ctx, lifted);
        },
        ExprKind::Tuple(ref mut vals) => {
            for val in vals.iter_mut() {
                lift_from_expr(val, ctx, lifted);
            };
        },
        ExprKind::Paren(box ref mut val) => {
            lift_from_expr(val, ctx, lifted);
        },
        ExprKind::If(box (ref mut cond, ref mut true_block, ref mut false_block)) => {
            lift_from_expr(cond, ctx, lifted);
            lift_from_block(true_block, ctx, lifted);
            if let Some(ref mut false_block) = false_block {
                lift_from_block(false_block, ctx, lifted);
            };
        },
        ExprKind::While(box (ref mut cond, ref mut do_block)) => {
            lift_from_expr(cond, ctx, lifted);
            lift_from_block(do_block, ctx, lifted);
        },
        ExprKind::Lambda(ref mut body) => {
            if let LambdaBody::Inline(box mut func) = mem::replace(body, LambdaBody::Lifted(SymId(!0))) {
                *body = LambdaBody::Lifted(func.sym_id.unwrap());

                lift_from_function(&mut func, ctx, lifted);
                lifted.push(func);
            } else {
                unreachable!();
            };
        },
        ExprKind::Int(_) => {},
        ExprKind::Bool(_) => {}
    };
}

fn lift_from_stmt(stmt: &mut Stmt, ctx: &mut AnalysisContext, lifted: &mut Vec<Function>) {
    match stmt.node {
        StmtKind::Let(_, ref mut val) => {
            lift_from_expr(val, ctx, lifted);
        },
        StmtKind::Return(ref mut val) => {
            lift_from_expr(val, ctx, lifted);
        },
        StmtKind::Assign(ref mut lhs, ref mut rhs) => {
            lift_from_expr(lhs, ctx, lifted);
            lift_from_expr(rhs, ctx, lifted);
        },
        StmtKind::Expr(ref mut val) => {
            lift_from_expr(val, ctx, lifted);
        }
    };
}

fn lift_from_block(b: &mut Block, ctx: &mut AnalysisContext, lifted: &mut Vec<Function>) {
    for stmt in b.stmts.iter_mut() {
        lift_from_stmt(stmt, ctx, lifted);
    };

    if let Some(ref mut result) = b.result {
        lift_from_expr(result, ctx, lifted);
    };
}

fn lift_from_function(f: &mut Function, ctx: &mut AnalysisContext, lifted: &mut Vec<Function>) {
    lift_from_block(&mut f.body, ctx, lifted);
}

pub fn lift_module(m: &mut Module, errors: &mut Vec<Error>) {
    let ctx = &mut AnalysisContext {
        types: &mut m.types,
        sym_defs: &mut m.syms,
        sym_refs: ScopedSymRefTable::new(),
        errors,
        fn_sym: SymId(!0)
    };

    let mut lifted = vec![];

    for f in m.funcs.iter_mut() {
        lift_from_function(f, ctx, &mut lifted);
    };

    m.funcs.append(&mut lifted);
}
