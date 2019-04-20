use std::collections::{HashMap, HashSet, VecDeque};
use std::collections::hash_map::Entry;
use std::fmt::{self, Debug, Display};
use std::hash::Hash;
use std::mem;

use itertools::{self, Itertools};
use smallvec::SmallVec;

use crate::bitvec::BitVec;
use crate::il::{IlBlock, IlBlockId, IlEndingInstruction, IlEndingInstructionKind, IlFunction, IlOperand, IlRegister, IlSpanId};
use crate::log::Log;
use super::flow_graph::FlowGraph;

pub struct AnalysisStructures {
    pub cfg: FlowGraph<IlBlockId>,
    pub liveness: LivenessGraph<IlBlockId>,
    pub defs: ReachingDefs,
    pub ebbs: ExtendedBlocks,
    pub dom: Dominance,
    pub loops: Loops
}

impl AnalysisStructures {
    pub fn for_func(f: &IlFunction) -> AnalysisStructures {
        AnalysisStructures {
            cfg: FlowGraph::for_func(f),
            liveness: LivenessGraph::new(),
            defs: ReachingDefs::new(),
            ebbs: ExtendedBlocks::new(),
            dom: Dominance::new(),
            loops: Loops::new()
        }
    }
}

pub struct BlockLivenessInfo {
    live_begin: HashSet<IlRegister>,
    live_end: HashSet<IlRegister>,
    pub gen: HashSet<IlRegister>,
    pub kill: HashSet<IlRegister>
}

impl BlockLivenessInfo {
    pub fn new(gen: HashSet<IlRegister>, kill: HashSet<IlRegister>) -> BlockLivenessInfo {
        BlockLivenessInfo {
            live_begin: gen.clone(),
            live_end: HashSet::new(),
            gen,
            kill
        }
    }
}

fn compute_block_liveness_effects(
    block: &IlBlock,
    _log: &mut Log,
    donor_live_end: HashSet<IlRegister>
) -> BlockLivenessInfo {
    let mut liveness = BlockLivenessInfo {
        live_begin: HashSet::new(),
        live_end: donor_live_end,
        gen: HashSet::new(),
        kill: HashSet::new()
    };

    liveness.live_end.clear();

    block.end_instr.node.for_operands(|o| {
        if let IlOperand::Register(reg) = *o {
            liveness.gen.insert(reg);
            liveness.kill.remove(&reg);

            liveness.live_begin.insert(reg);
        };
    });

    for instr in block.instrs.iter().rev() {
        if let Some(reg) = instr.node.target() {
            liveness.gen.remove(&reg);
            liveness.kill.insert(reg);

            liveness.live_begin.remove(&reg);
        };

        instr.node.for_operands(|o| {
            if let IlOperand::Register(reg) = *o {
                liveness.gen.insert(reg);
                liveness.kill.remove(&reg);

                liveness.live_begin.insert(reg);
            };
        });
    };

    liveness
}

fn set_block_liveness<T: Debug + Display + Copy + Eq + Hash>(
    id: T,
    all_liveness: &mut HashMap<T, BlockLivenessInfo>,
    new_live_begin: HashSet<IlRegister>,
    cfg: &FlowGraph<T>,
    worklist: &mut VecDeque<T>,
    log: &mut Log
) {
    log_write!(log, "{}: Recomputed, live_begin = {{ ", id);
    for &reg in new_live_begin.iter() {
        log_write!(log, "{} ", reg);
    };
    log_writeln!(log, "}}");

    for &prev_id in cfg.get(id).rev_edges.iter() {
        let prev_liveness = all_liveness.get_mut(&prev_id).unwrap();
        let mut updated = false;

        for &reg in new_live_begin.iter() {
            updated = prev_liveness.live_end.insert(reg) || updated;
        };

        if updated {
            log_write!(log, "  {}: Updated, live_end = {{ ", prev_id);
            for &reg in prev_liveness.live_end.iter() {
                log_write!(log, "{} ", reg);
            };
            log_writeln!(log, "}}");

            if !worklist.contains(&prev_id) {
                worklist.push_back(prev_id);
            };
        };
    };
    all_liveness.get_mut(&id).unwrap().live_begin = new_live_begin;
}

fn apply_block_liveness_effects<T: Debug + Display + Copy + Eq + Hash>(
    id: T,
    all_liveness: &mut HashMap<T, BlockLivenessInfo>,
    cfg: &FlowGraph<T>,
    worklist: &mut VecDeque<T>,
    log: &mut Log
) {
    let liveness = all_liveness.get(&id).unwrap();
    let mut new_live_begin = liveness.live_end.clone();

    for &reg in liveness.kill.iter() {
        new_live_begin.remove(&reg);
    };

    for &reg in liveness.gen.iter() {
        new_live_begin.insert(reg);
    };

    if new_live_begin != liveness.live_begin {
        set_block_liveness(id, all_liveness, new_live_begin, cfg, worklist, log);
    } else {
        log_writeln!(log, "{}: Recomputed, no update", id);
    };
}

lazy_static! {
    static ref EMPTY_LIVE: HashSet<IlRegister> = HashSet::new();
}

pub struct LivenessGraph<T: Debug + Display + Copy + Eq + Hash> {
    live: HashMap<T, HashSet<IlRegister>>,
    global_regs: BitVec<IlRegister>
}

impl <T: Debug + Display + Copy + Eq + Hash> LivenessGraph<T> {
    pub fn new() -> LivenessGraph<T> {
        LivenessGraph { live: HashMap::new(), global_regs: BitVec::new() }
    }

    pub fn recompute_from_effects(&mut self, effects: Vec<(T, BlockLivenessInfo)>, cfg: &FlowGraph<T>, log: &mut Log) {
        self.global_regs.clear();

        let mut all_liveness = HashMap::new();
        let order = effects.iter().map(|&(id, _)| id).collect_vec();

        for (id, liveness) in effects {
            for &reg in liveness.gen.iter() {
                self.global_regs.set(reg, true);
            };

            log_write!(log, "{}: kill = {{ ", id);
            for &reg in liveness.kill.iter() {
                log_write!(log, "{} ", reg);
            };
            log_write!(log, "}}, gen = {{ ");
            for &reg in liveness.gen.iter() {
                log_write!(log, "{} ", reg);
            };
            log_writeln!(log, "}}");

            all_liveness.insert(id, liveness);
        };

        log_writeln!(log);

        let mut worklist = VecDeque::with_capacity(order.len());

        for id in order.into_iter().rev() {
            let new_live_begin = mem::replace(
                &mut all_liveness.get_mut(&id).unwrap().live_begin,
                HashSet::new()
            );
            set_block_liveness(id, &mut all_liveness, new_live_begin, cfg, &mut worklist, log);
        };

        while let Some(next) = worklist.pop_front() {
            apply_block_liveness_effects(next, &mut all_liveness, cfg, &mut worklist, log);
        };

        log_writeln!(log);

        self.live.clear();

        for (id, liveness) in all_liveness {
            log_write!(log, "{}: live_end = {{ ", id);
            for &reg in liveness.live_end.iter() {
                log_write!(log, "{} ", reg);
            };
            log_writeln!(log, "}}");
            self.live.insert(id, liveness.live_end);
        };
    }

    pub fn get(&self, id: T) -> &HashSet<IlRegister> {
        self.live.get(&id).unwrap_or(&EMPTY_LIVE)
    }

    pub fn get_mut(&mut self, id: T) -> &mut HashSet<IlRegister> {
        self.live.entry(id).or_insert_with(HashSet::new)
    }

    pub fn global_regs(&self) -> &BitVec<IlRegister> {
        &self.global_regs
    }
}

impl <T: Debug + Display + Copy + Eq + Hash> Default for LivenessGraph<T> {
    fn default() -> Self {
        Self::new()
    }
}

impl LivenessGraph<IlBlockId> {
    pub fn recompute_global_regs(&mut self, func: &IlFunction, _log: &mut Log) {
        self.global_regs.clear();

        let mut defined = BitVec::new();

        for &id in func.block_order.iter() {
            defined.clear();
            for i in func.blocks[&id].instrs.iter() {
                i.node.for_operands(|o| if let IlOperand::Register(reg) = *o {
                    if !defined.get(reg) {
                        self.global_regs.set(reg, true);
                    };
                });

                if let Some(target) = i.node.target() {
                    defined.set(target, true);
                };
            };
        };
    }

    pub fn recompute(&mut self, func: &IlFunction, cfg: &FlowGraph<IlBlockId>, log: &mut Log) {
        log_writeln!(log, "\n===== LIVENESS ANALYSIS =====\n");

        let effects = func.block_order.iter().map(|&id| {
            (id, compute_block_liveness_effects(
                &func.blocks[&id],
                log,
                self.live.remove(&id).unwrap_or_else(HashSet::new)
            ))
        }).collect_vec();

        self.recompute_from_effects(effects, cfg, log);
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Def(pub IlBlockId, pub usize);

impl Def {
    pub fn dummy() -> Def {
        Def(IlBlockId::dummy(), 0)
    }

    pub fn is_dummy(&self) -> bool {
        self.0 == IlBlockId::dummy()
    }
}

struct BlockReachingDefInfo {
    defs_begin: HashMap<IlRegister, SmallVec<[Def; 1]>>,
    defs_end: HashMap<IlRegister, SmallVec<[Def; 1]>>,
    gen: Vec<(IlRegister, Def)>
}

fn compute_block_reaching_def_effects(
    id: IlBlockId,
    block: &IlBlock,
    global_regs: &BitVec<IlRegister>,
    _log: &mut Log,
    donor_defs_begin: HashMap<IlRegister, SmallVec<[Def; 1]>>
) -> BlockReachingDefInfo {
    let mut defs = BlockReachingDefInfo {
        defs_begin: donor_defs_begin,
        defs_end: HashMap::new(),
        gen: vec![]
    };
    defs.defs_begin.clear();

    for (i, instr) in block.instrs.iter().enumerate() {
        if let Some(reg) = instr.node.target() {
            if !global_regs.get(reg) {
                continue;
            };

            let def = Def(id, i);
            if defs.defs_end.insert(reg, smallvec![def]).is_some() {
                defs.gen.drain_filter(|&mut (r, _)| r == reg);
            };
            defs.gen.push((reg, def));
        };
    };

    defs
}

fn set_block_reaching_defs(
    id: IlBlockId,
    all_defs: &mut HashMap<IlBlockId, BlockReachingDefInfo>,
    new_defs_end: HashMap<IlRegister, SmallVec<[Def; 1]>>,
    cfg: &FlowGraph<IlBlockId>,
    worklist: &mut VecDeque<IlBlockId>,
    log: &mut Log
) {
    log_write!(log, "{}: Recomputed, defs_end = {{ ", id);
    for (&reg, defs) in new_defs_end.iter() {
        log_write!(log, "({}: [ ", reg);
        for &Def(def_blk, def_off) in defs.iter() {
            log_write!(log, "{}:{} ", def_blk, def_off);
        };
        log_write!(log, "]) ");
    };
    log_writeln!(log, "}}");

    for &next_id in cfg.get(id).edges.iter() {
        let next_defs = all_defs.get_mut(&next_id).unwrap();
        let mut updated = false;

        for (&reg, defs) in new_defs_end.iter() {
            let next_reg_defs = next_defs.defs_begin.entry(reg).or_insert_with(|| smallvec![]);

            for &def in defs.iter() {
                let already_exists = next_reg_defs.iter().any(|&old_def| old_def == def);

                if !already_exists {
                    next_reg_defs.push(def);
                    updated = true;
                };
            };
        };

        if updated {
            log_write!(log, "  {}: Updated, defs_begin = {{ ", next_id);
            for (&reg, defs) in next_defs.defs_begin.iter() {
                log_write!(log, "({}: [ ", reg);
                for &Def(def_blk, def_off) in defs.iter() {
                    log_write!(log, "{}:{} ", def_blk, def_off);
                };
                log_write!(log, "]) ");
            };
            log_writeln!(log, "}}");

            if !worklist.contains(&next_id) {
                worklist.push_back(next_id);
            };
        };
    };
    all_defs.get_mut(&id).unwrap().defs_end = new_defs_end;
}

fn apply_block_reaching_def_effects(
    id: IlBlockId,
    all_defs: &mut HashMap<IlBlockId, BlockReachingDefInfo>,
    cfg: &FlowGraph<IlBlockId>,
    worklist: &mut VecDeque<IlBlockId>,
    log: &mut Log
) {
    let defs = all_defs.get(&id).unwrap();
    let mut new_defs_end = defs.defs_begin.clone();

    for &(reg, def) in defs.gen.iter() {
        new_defs_end.insert(reg, smallvec![def]);
    };

    if new_defs_end != defs.defs_end {
        set_block_reaching_defs(id, all_defs, new_defs_end, cfg, worklist, log);
    } else {
        log_writeln!(log, "{}: Recomputed, no update", id);
    };
}

lazy_static! {
    static ref EMPTY_DEFS: HashMap<IlRegister, SmallVec<[Def; 1]>> = HashMap::new();
}

pub struct ReachingDefs {
    defs: HashMap<IlBlockId, HashMap<IlRegister, SmallVec<[Def; 1]>>>
}

impl ReachingDefs {
    pub fn new() -> ReachingDefs {
        ReachingDefs { defs: HashMap::new() }
    }

    pub fn recompute(
        &mut self,
        func: &IlFunction,
        cfg: &FlowGraph<IlBlockId>,
        global_regs: &BitVec<IlRegister>,
        log: &mut Log
    ) {
        log_writeln!(log, "\n===== REACHING DEFS ANALYSIS =====\n");

        let mut all_defs = HashMap::new();

        for &id in func.block_order.iter() {
            let defs = compute_block_reaching_def_effects(
                id,
                &func.blocks[&id],
                global_regs,
                log,
                self.defs.remove(&id).unwrap_or_else(HashMap::new)
            );

            log_write!(log, "{}: gen = {{ ", id);
            for &(reg, Def(def_blk, def_off)) in defs.gen.iter() {
                log_write!(log, "({}: {}:{}) ", reg, def_blk, def_off);
            };
            log_writeln!(log, "}}");

            all_defs.insert(id, defs);
        };

        let first_block = func.block_order[0];
        let first_block_defs = all_defs.get_mut(&first_block).unwrap();

        log_write!(log, "{}: Added param dummies, defs_begin = {{ ", first_block);
        for &reg in func.reg_map.params() {
            first_block_defs.defs_begin.insert(reg, smallvec![Def::dummy()]);
            log_write!(log, "({}: [ {}:0 ]) ", reg, IlBlockId::dummy());

            match first_block_defs.defs_end.entry(reg) {
                Entry::Occupied(_) => {}, // Must have been in gen set, do not add dummy
                Entry::Vacant(e) => {
                    e.insert(smallvec![Def::dummy()]);
                }
            };
        };
        log_writeln!(log, "}}");

        log_writeln!(log);

        let mut worklist = VecDeque::new();

        for &id in func.block_order.iter() {
            let new_defs_end = mem::replace(
                &mut all_defs.get_mut(&id).unwrap().defs_end,
                HashMap::new()
            );
            set_block_reaching_defs(id, &mut all_defs, new_defs_end, cfg, &mut worklist, log);
        };

        while let Some(next) = worklist.pop_front() {
            apply_block_reaching_def_effects(next, &mut all_defs, cfg, &mut worklist, log);
        };

        log_writeln!(log);

        for (id, defs) in all_defs {
            log_write!(log, "{}: defs_begin = {{ ", id);
            for (&reg, defs) in defs.defs_begin.iter() {
                log_write!(log, "({}: [ ", reg);
                for &Def(def_blk, def_off) in defs.iter() {
                    log_write!(log, "{}:{} ", def_blk, def_off);
                };
                log_write!(log, "]) ");
            };
            log_writeln!(log, "}}");
            self.defs.insert(id, defs.defs_begin);
        };
    }

    pub fn get(&self, id: IlBlockId) -> &HashMap<IlRegister, SmallVec<[Def; 1]>> {
        self.defs.get(&id).unwrap_or(&EMPTY_DEFS)
    }
}

impl Default for ReachingDefs {
    fn default() -> Self {
        Self::new()
    }
}

#[derive(Debug, Clone)]
pub struct ExtendedBlock {
    pub blocks: SmallVec<[IlBlockId; 1]>
}

impl ExtendedBlock {
    pub fn new() -> ExtendedBlock {
        ExtendedBlock { blocks: smallvec![] }
    }
}

impl Default for ExtendedBlock {
    fn default() -> Self {
        Self::new()
    }
}

pub struct ExtendedBlocks {
    blocks: Vec<ExtendedBlock>
}

impl ExtendedBlocks {
    pub fn new() -> ExtendedBlocks {
        ExtendedBlocks { blocks: vec![] }
    }

    pub fn clear(&mut self) {
        self.blocks.clear();
    }

    pub fn recompute(&mut self, func: &IlFunction, cfg: &FlowGraph<IlBlockId>, log: &mut Log) {
        fn visit(
            id: IlBlockId,
            ebbs: &mut ExtendedBlocks,
            mut ebb: ExtendedBlock,
            cfg: &FlowGraph<IlBlockId>,
            extensions: &BitVec<IlBlockId>
        ) {
            ebb.blocks.push(id);

            let edges = &cfg.get(id).edges;
            let mut extended = false;

            for &target in edges.iter().skip(1) {
                if extensions.get(target) {
                    visit(target, ebbs, ebb.clone(), cfg, extensions);
                    extended = true;
                };
            };

            if let Some(&target) = edges.first() {
                if extensions.get(target) {
                    visit(target, ebbs, ebb, cfg, extensions);
                } else {
                    ebbs.blocks.push(ebb);
                };
            } else if !extended {
                ebbs.blocks.push(ebb);
            };
        }

        log_writeln!(log, "\n===== BASIC BLOCK EXTENSION =====\n");

        self.clear();

        let mut extensions = BitVec::new();
        let mut starts = vec![func.block_order[0]];

        for &id in func.block_order.iter().skip(1) {
            if cfg.get(id).rev_edges.len() == 1 {
                extensions.set(id, true);
            } else {
                starts.push(id);
            };
        };

        for id in starts {
            visit(id, self, ExtendedBlock::new(), cfg, &extensions);
        };

        log_write!(log, "{}", self);
    }

    pub fn iter(&self) -> impl Iterator<Item=&ExtendedBlock> {
        self.blocks.iter()
    }
}

impl Default for ExtendedBlocks {
    fn default() -> Self {
        Self::new()
    }
}

impl Display for ExtendedBlocks {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        for ebb in self.iter() {
            write!(f, "[ ")?;
            for id in ebb.blocks.iter() {
                write!(f, "{} ", id)?;
            };
            writeln!(f, "]")?;
        };

        Result::Ok(())
    }
}

pub struct Dominance {
    dom: HashMap<IlBlockId, BitVec<IlBlockId>>
}

impl Dominance {
    pub fn new() -> Dominance {
        Dominance { dom: HashMap::new() }
    }

    pub fn recompute(&mut self, func: &IlFunction, cfg: &FlowGraph<IlBlockId>, log: &mut Log) {
        let mut worklist = VecDeque::new();

        log_writeln!(log, "\n===== DOMINANCE  =====\n");

        let start_block = func.block_order[0];

        let mut old_bv = BitVec::new();
        let mut bv = BitVec::new();

        for &id in func.block_order.iter() {
            bv.set(id, true);
        };

        for (i, &id) in func.block_order.iter().enumerate() {
            let mut dom = self.dom.entry(id);
            let dom = match dom {
                Entry::Occupied(ref mut e) => e.get_mut(),
                Entry::Vacant(e) => e.insert(BitVec::new())
            };
            if i == 0 {
                dom.clear();
                dom.set(id, true);
            } else {
                dom.clone_from(&bv);
            };
            worklist.push_back(id);
        };

        while let Some(id) = worklist.pop_front() {
            if id == start_block {
                continue;
            };

            let node = cfg.get(id);
            let mut preds = node.rev_edges.iter().filter(|&&pred_id| pred_id != id);

            mem::swap(self.dom.get_mut(&id).unwrap(), &mut bv);
            old_bv.clone_from(&bv);

            bv.clear();

            if let Some(&pred_id) = preds.next() {
                bv.union(&self.dom[&pred_id]);
            };

            for &pred_id in preds {
                bv.intersect(&self.dom[&pred_id]);
            };

            bv.set(id, true);

            if bv != old_bv {
                for &succ_id in node.edges.iter().filter(|&&succ_id| succ_id != id) {
                    if !worklist.contains(&succ_id) {
                        worklist.push_back(succ_id);
                    };
                };
            };

            mem::swap(self.dom.get_mut(&id).unwrap(), &mut bv);
        };

        for &id in func.block_order.iter() {
            log_write!(log, "{} dominated by {{ ", id);

            for dom_id in self.dom[&id].iter() {
                log_write!(log, "{} ", dom_id);
            };

            log_writeln!(log, "}}");
        };
    }

    pub fn get(&self, id: IlBlockId) -> &BitVec<IlBlockId> {
        &self.dom[&id]
    }
}

impl Default for Dominance {
    fn default() -> Self {
        Self::new()
    }
}

pub struct Loop {
    pub pre_header: IlBlockId,
    pub header: IlBlockId,
    pub blocks: SmallVec<[IlBlockId; 1]>,
    pub back_edges: SmallVec<[IlBlockId; 1]>
}

impl Loop {
    pub fn new(header: IlBlockId, back_edge: IlBlockId) -> Loop {
        Loop {
            pre_header: IlBlockId::dummy(),
            header,
            blocks: smallvec![],
            back_edges: smallvec![back_edge]
        }
    }

    pub fn blocks<'a>(&'a self) -> impl Iterator<Item=IlBlockId> + 'a {
        itertools::repeat_n(self.header, 1)
            .chain(self.blocks.iter().cloned())
    }
}

pub struct Loops {
    pub loops: Vec<Loop>
}

impl Loops {
    pub fn new() -> Loops {
        Loops { loops: vec![] }
    }

    pub fn recompute(
        &mut self,
        func: &mut IlFunction,
        cfg: &mut FlowGraph<IlBlockId>,
        dom: &Dominance,
        log: &mut Log
    ) {
        log_writeln!(log, "\n===== LOOP DETECTION =====\n");

        let mut by_header: HashMap<IlBlockId, usize> = HashMap::new();

        for &id in func.block_order.iter() {
            let node = cfg.get(id);
            let dom = dom.get(id);

            for &next_id in node.edges.iter() {
                if dom.get(next_id) {
                    log_writeln!(log, "Found back edge from {} to {}", id, next_id);

                    match by_header.entry(next_id) {
                        Entry::Occupied(e) => {
                            self.loops[*e.get()].back_edges.push(id);
                        },
                        Entry::Vacant(e) => {
                            e.insert(self.loops.len());
                            self.loops.push(Loop::new(next_id, id));
                        }
                    };
                };
            };
        };

        let mut fixed = false;

        for l in self.loops.iter_mut() {
            log_writeln!(log, "Processing loop with header {}", l.header);

            let header_node = cfg.get(l.header);

            if l.header == func.block_order[0] || header_node.rev_edges.len() != l.back_edges.len() + 1 {
                log_writeln!(log, "  No pre-header found");
            } else {
                let pre_header = header_node.rev_edges.iter()
                    .cloned()
                    .find(|&id| l.back_edges.iter().find(|&&id2| id == id2).is_none())
                    .unwrap();

                let pre_header_node = cfg.get(pre_header);

                if pre_header_node.edges.len() == 1 {
                    l.pre_header = pre_header;
                    log_writeln!(log, "  Detected {} as pre-header", pre_header);
                } else {
                    log_writeln!(log, "  No pre-header found ({} is a conditional jump)", pre_header);
                };
            };

            fn add_block_to_loop(
                l: &mut Loop,
                id: IlBlockId,
                func: &IlFunction,
                cfg: &FlowGraph<IlBlockId>
            ) {
                if l.blocks().any(|in_loop| in_loop == id) {
                    return;
                };

                l.blocks.push(id);

                for &pred_id in cfg.get(id).rev_edges.iter() {
                    add_block_to_loop(l, pred_id, func, cfg);
                };
            }

            for i in 0..l.back_edges.len() {
                let id = l.back_edges[i];
                add_block_to_loop(l, id, func, cfg);
            };

            log_write!(log, "  Detected blocks: [ ");

            for id in l.blocks() {
                log_write!(log, "{} ", id);
            };

            log_writeln!(log, "]");

            if l.pre_header == IlBlockId::dummy() {
                let to_fixup = header_node.rev_edges.iter()
                    .cloned()
                    .filter(|&id| l.back_edges.iter().find(|&&id2| id == id2).is_none())
                    .collect_vec();

                let mut pre_header_block = IlBlock::new();
                pre_header_block.end_instr = IlEndingInstruction::new(
                    IlEndingInstructionKind::Jump(l.header),
                    IlSpanId::dummy()
                );

                l.pre_header = func.add_block(pre_header_block);
                let pos = func.block_order.iter().enumerate()
                    .find(|&(_, &id)| l.blocks().any(|id2| id == id2))
                    .unwrap().0;

                log_writeln!(log, "  Inserting new pre-header {} before {}", l.pre_header, func.block_order[pos]);
                func.block_order.insert(pos, l.pre_header);

                let header_node = cfg.get_mut(l.header);
                header_node.rev_edges.clear();
                header_node.rev_edges.extend(l.back_edges.iter().cloned());
                header_node.rev_edges.push(l.pre_header);

                for &id in to_fixup.iter() {
                    log_writeln!(log, "    Fixing up {}", id);

                    let fixup_node = cfg.get_mut(id);

                    for succ in fixup_node.edges.iter_mut() {
                        if *succ == l.header {
                            *succ = l.pre_header;
                        };
                    };

                    let fixup_block = func.blocks.get_mut(&id).unwrap();

                    if fixup_block.end_instr.node.target_block() == Some(l.header) {
                        fixup_block.end_instr.node.set_target_block(l.pre_header);
                    };
                };

                let mut preheader_node = cfg.add_node(l.pre_header);
                preheader_node.rev_edges = to_fixup;
                preheader_node.edges.push(l.header);

                fixed = true;
            };
        };

        if fixed {
            log_writeln!(log, "\n===== AFTER LOOP DETECTION =====\n\n{}\n{}", func, cfg.pretty(func));
        };
    }
}

impl Default for Loops {
    fn default() -> Self {
        Self::new()
    }
}
