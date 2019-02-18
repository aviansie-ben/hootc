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

use std::io::Write;

use itertools::Itertools;

use crate::il::{IlFunction, IlProgram};

pub fn optimize_function_pre_inlining(func: &mut IlFunction, w: &mut Write) {
    writeln!(w, "\n===== RUNNING PRE-INLINING OPTIMIZATIONS FOR FUNCTION {} =====\n", func.sym).unwrap();

    let mut cfg = flow_graph::FlowGraph::for_func(func);
    let mut liveness = analysis::LivenessGraph::new();
    let mut defs = analysis::ReachingDefs::new();
    let mut ebbs = analysis::ExtendedBlocks::new();
    let mut dom = analysis::Dominance::new();
    let mut loops = analysis::Loops::new();

    do_merge_blocks_group(func, &mut cfg, w);
    do_constant_fold_group(func, &mut cfg, &mut liveness, &mut defs, &mut ebbs, w);
    do_loop_opt_group(func, &mut cfg, &mut liveness, &mut defs, &mut ebbs, &mut dom, &mut loops, w);

    writeln!(w, "\n===== AFTER PRE-INLINING OPTIMIZATIONS =====\n\n{}", func).unwrap();
}

pub fn optimize_function_post_inlining(func: &mut IlFunction, w: &mut Write) {
    writeln!(w, "\n===== RUNNING POST-INLINING OPTIMIZATIONS FOR FUNCTION {} =====\n", func.sym).unwrap();

    let mut cfg = flow_graph::FlowGraph::for_func(func);
    let mut liveness = analysis::LivenessGraph::new();
    let mut defs = analysis::ReachingDefs::new();
    let mut ebbs = analysis::ExtendedBlocks::new();
    let mut dom = analysis::Dominance::new();
    let mut loops = analysis::Loops::new();

    do_merge_blocks_group(func, &mut cfg, w);
    do_constant_fold_group(func, &mut cfg, &mut liveness, &mut defs, &mut ebbs, w);
    do_loop_opt_group(func, &mut cfg, &mut liveness, &mut defs, &mut ebbs, &mut dom, &mut loops, w);

    writeln!(w, "\n===== AFTER POST-INLINING OPTIMIZATIONS =====\n\n{}", func).unwrap();
}

pub fn optimize_program(prog: &mut IlProgram, w: &mut Write) {
    for (_, func) in prog.funcs.iter_mut() {
        optimize_function_pre_inlining(func, w);
    };

    let mut funcs = prog.funcs.keys().cloned().collect_vec();
    let mut sites = vec![];

    while !funcs.is_empty() {
        funcs.drain_filter(|sym| {
            inliner::do_inlining_decision_phase(prog, &prog.funcs[&sym], &mut sites, w);

            if !sites.is_empty() {
                inliner::do_inlining_expansion_phase(prog.funcs.get_mut(&sym).unwrap(), &mut sites, w);
                assert!(sites.is_empty());
                false
            } else {
                true
            }
        });
    };

    for (_, func) in prog.funcs.iter_mut() {
        optimize_function_post_inlining(func, w);
    };
}
