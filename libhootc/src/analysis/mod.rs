use ::sym::{SymDefTable, ScopedSymRefTable};
use ::types::TypeTable;

pub mod error;
pub mod typecheck;

pub struct AnalysisContext<'a> {
    pub types: &'a mut TypeTable,
    pub sym_defs: &'a mut SymDefTable,
    pub sym_refs: ScopedSymRefTable,
    pub errors: &'a mut Vec<error::Error>,
    pub fn_id: u32,
    pub next_fn_id: u32
}
