pub mod analysis;
mod block_merge;
mod common_subexpr;
mod constant;
mod dead_code;
pub mod flow_graph;
mod inliner;
mod loops;

pub use self::block_merge::{do_merge_blocks_group, eliminate_redundant_jumps, merge_blocks};
pub use self::common_subexpr::{eliminate_local_common_subexpressions};
pub use self::constant::{do_constant_fold_group, propagate_and_fold_constants, propagate_copies_locally};
pub use self::dead_code::{eliminate_dead_blocks, eliminate_dead_stores};
pub use self::loops::{do_loop_opt_group};

use itertools::Itertools;

use crate::il::{IlFunction, IlProgram};
use crate::log::Log;

pub fn optimize_function_pre_inlining(func: &mut IlFunction, log: &mut Log) {
    log_writeln!(log, "\n===== RUNNING PRE-INLINING OPTIMIZATIONS FOR FUNCTION {} =====\n", func.sym);

    let mut structs = analysis::AnalysisStructures::for_func(func);

    do_merge_blocks_group(func, &mut structs.cfg, log);
    do_constant_fold_group(func, &mut structs, log);
    do_loop_opt_group(func, &mut structs, log);

    log_writeln!(log, "\n===== AFTER PRE-INLINING OPTIMIZATIONS =====\n\n{}", func);
}

pub fn optimize_function_post_inlining(func: &mut IlFunction, log: &mut Log) {
    log_writeln!(log, "\n===== RUNNING POST-INLINING OPTIMIZATIONS FOR FUNCTION {} =====\n", func.sym);

    let mut structs = analysis::AnalysisStructures::for_func(func);

    do_merge_blocks_group(func, &mut structs.cfg, log);
    do_constant_fold_group(func, &mut structs, log);
    do_loop_opt_group(func, &mut structs, log);

    log_writeln!(log, "\n===== AFTER POST-INLINING OPTIMIZATIONS =====\n\n{}", func);
}

pub fn optimize_program(prog: &mut IlProgram, log: &mut Log) {
    for (_, func) in prog.funcs.iter_mut() {
        optimize_function_pre_inlining(func, log);
    };

    let mut funcs = prog.funcs.keys().cloned().collect_vec();
    let mut sites = vec![];

    funcs.sort_by_key(|sym| sym.0);

    while !funcs.is_empty() {
        funcs.drain_filter(|sym| {
            inliner::do_inlining_decision_phase(prog, &prog.funcs[&sym], &mut sites, log);

            if !sites.is_empty() {
                inliner::do_inlining_expansion_phase(prog.funcs.get_mut(&sym).unwrap(), &mut sites, log);
                assert!(sites.is_empty());
                false
            } else {
                true
            }
        });
    };

    for (_, func) in prog.funcs.iter_mut() {
        optimize_function_post_inlining(func, log);
    };
}
