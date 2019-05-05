use ::ast::Module;
use ::sym::{SymDefTable, SymId, ScopedSymRefTable};
use ::types::TypeTable;

#[macro_use] pub mod error;

pub mod lift;
pub mod resolve;
pub mod typecheck;

pub struct AnalysisContext<'a> {
    pub types: &'a mut TypeTable,
    pub sym_defs: &'a mut SymDefTable,
    pub sym_refs: ScopedSymRefTable,
    pub errors: &'a mut Vec<error::Error>,
    pub fn_sym: SymId
}

pub fn analyze_module(m: &mut Module, errors: &mut Vec<error::Error>) {
    resolve::resolve_module(m, errors);
    typecheck::analyze_module(m, errors);
    lift::lift_module(m, errors);
}
