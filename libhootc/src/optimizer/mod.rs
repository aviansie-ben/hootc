pub mod analysis;
mod block_merge;
mod constant;
mod dead_code;
pub mod flow_graph;
mod loops;

pub use self::block_merge::{do_merge_blocks_group, eliminate_redundant_jumps, merge_blocks};
pub use self::constant::{do_constant_fold_group, propagate_and_fold_constants, propagate_copies_locally};
pub use self::dead_code::{eliminate_dead_blocks, eliminate_dead_stores};
pub use self::loops::{do_loop_opt_group};

use std::io::Write;

use crate::il::{IlFunction, IlProgram};

pub fn optimize_function_pre_inlining(func: &mut IlFunction, w: &mut Write) {
    let mut cfg = flow_graph::FlowGraph::for_func(func);
    let mut liveness = analysis::LivenessGraph::new();
    let mut defs = analysis::ReachingDefs::new();
    let mut ebbs = analysis::ExtendedBlocks::new();
    let mut dom = analysis::Dominance::new();
    let mut loops = analysis::Loops::new();

    do_merge_blocks_group(func, &mut cfg, w);
    do_constant_fold_group(func, &mut cfg, &mut liveness, &mut defs, &mut ebbs, w);
    do_loop_opt_group(func, &mut cfg, &mut liveness, &mut defs, &mut ebbs, &mut dom, &mut loops, w);
}

pub fn optimize_program(prog: &mut IlProgram, w: &mut Write) {
    for (_, func) in prog.funcs.iter_mut() {
        optimize_function_pre_inlining(func, w);
    };
}
