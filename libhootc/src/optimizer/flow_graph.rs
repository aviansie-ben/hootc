use std::collections::HashMap;
use std::fmt::{self, Debug, Display};
use std::hash::Hash;
use std::mem;

use itertools::Itertools;

use crate::il::{IlBlockId, IlEndingInstructionKind, IlFunction};

#[derive(Debug)]
pub struct FlowGraphNode<T> {
    pub edges: Vec<T>,
    pub rev_edges: Vec<T>,
    pub returns: bool
}

impl <T> FlowGraphNode<T> {
    pub fn new() -> FlowGraphNode<T> {
        FlowGraphNode { edges: vec![], rev_edges: vec![], returns: false }
    }
}

impl <T> Default for FlowGraphNode<T> {
    fn default() -> Self {
        Self::new()
    }
}

#[derive(Debug)]
pub struct FlowGraph<T: Hash + Eq + Copy + Debug> {
    nodes: HashMap<T, FlowGraphNode<T>>,
    returning_nodes: Vec<T>
}

impl <T: Hash + Eq + Copy + Debug> FlowGraph<T> {
    pub fn new() -> FlowGraph<T> {
        FlowGraph { nodes: HashMap::new(), returning_nodes: vec![] }
    }

    pub fn add_node(&mut self, id: T) -> &mut FlowGraphNode<T> {
        self.nodes.insert(id, FlowGraphNode::new());
        self.get_mut(id)
    }

    pub fn remove_node(&mut self, id: T) -> FlowGraphNode<T> {
        self.nodes.remove(&id).unwrap()
    }

    pub fn merge_nodes_back(&mut self, from: T, to: T) {
        let to_node = self.nodes.remove(&to).unwrap();

        for &new_to in to_node.edges.iter() {
            mem::replace(
                self.nodes.get_mut(&new_to).unwrap().rev_edges.iter_mut().find(|&&mut id| id == to).unwrap(),
                from
            );
        };

        let from_node = self.nodes.get_mut(&from).unwrap();

        if to_node.returns {
            from_node.returns = true;
            mem::replace(
                self.returning_nodes.iter_mut().find(|&&mut id| id == to).unwrap(),
                from
            );
        };

        from_node.edges = to_node.edges;
    }

    pub fn merge_nodes_forward(&mut self, from: T, to: T) {
        let from_node = self.nodes.remove(&from).unwrap();

        for &new_from in from_node.rev_edges.iter() {
            mem::replace(
                self.nodes.get_mut(&new_from).unwrap().edges.iter_mut().find(|&&mut id| id == from).unwrap(),
                to
            );
        };

        let to_node = self.nodes.get_mut(&to).unwrap();

        to_node.rev_edges.remove_item(&from);
        to_node.rev_edges.extend(from_node.rev_edges.into_iter());
    }

    pub fn add_edge(&mut self, from: T, to: T) {
        self.nodes.get_mut(&from).unwrap().edges.push(to);
        self.nodes.get_mut(&to).unwrap().rev_edges.push(from);
    }

    pub fn add_return_edge(&mut self, from: T) {
        self.nodes.get_mut(&from).unwrap().returns = true;
        self.returning_nodes.push(from);
    }

    pub fn remove_edge(&mut self, from: T, to: T) {
        self.nodes.get_mut(&from).unwrap().edges.remove_item(&to);
        self.nodes.get_mut(&to).unwrap().rev_edges.remove_item(&from);
    }

    pub fn remove_return_edge(&mut self, from: T) {
        self.nodes.get_mut(&from).unwrap().returns = false;
        self.returning_nodes.remove_item(&from);
    }

    pub fn clear_edges_from(&mut self, from: T) {
        let mut edges = vec![];

        mem::swap(&mut self.nodes.get_mut(&from).unwrap().edges, &mut edges);

        for e in edges.drain(..) {
            self.nodes.get_mut(&e).unwrap().rev_edges.remove_item(&from);
        };

        let node = self.nodes.get_mut(&from).unwrap();

        if node.returns {
            node.returns = false;
            self.returning_nodes.remove_item(&from);
        };

        // It's not strictly necessary to swap back, but it allows memory previously allocated by
        // edges to be reused.
        mem::swap(&mut node.edges, &mut edges);
    }

    pub fn returning_nodes(&self) -> &[T] {
        &self.returning_nodes[..]
    }

    pub fn try_get(&self, id: T) -> Option<&FlowGraphNode<T>> {
        self.nodes.get(&id)
    }

    pub fn get(&self, id: T) -> &FlowGraphNode<T> {
        self.try_get(id).unwrap()
    }

    pub fn try_get_mut(&mut self, id: T) -> Option<&mut FlowGraphNode<T>> {
        self.nodes.get_mut(&id)
    }

    pub fn get_mut(&mut self, id: T) -> &mut FlowGraphNode<T> {
        self.try_get_mut(id).unwrap()
    }
}

impl <T: Hash + Eq + Copy + Debug> Default for FlowGraph<T> {
    fn default() -> Self {
        Self::new()
    }
}

impl FlowGraph<IlBlockId> {
    pub fn for_func(f: &IlFunction) -> FlowGraph<IlBlockId> {
        let mut graph = FlowGraph::new();
        graph.recompute(f);

        graph
    }

    pub fn recompute(&mut self, f: &IlFunction) {
        let mut nodes_to_reuse = self.nodes.drain().map(|(_, n)| n).collect_vec();
        let mut alloc_node = || {
            if let Some(mut n) = nodes_to_reuse.pop() {
                n.edges.clear();
                n.rev_edges.clear();
                n.returns = false;
                n
            } else {
                FlowGraphNode::new()
            }
        };

        self.returning_nodes.clear();

        for &id in f.block_order.iter() {
            self.nodes.insert(id, alloc_node());
        };

        for (&prev_id, &next_id) in f.block_order.iter().tuple_windows() {
            let prev = &f.blocks[&prev_id];

            if matches!(prev.end_instr.node, IlEndingInstructionKind::Return(_)) {
                self.add_return_edge(prev_id);
            };

            if prev.end_instr.node.can_fall_through() {
                self.add_edge(prev_id, next_id);
            };

            if let Some(target) = prev.end_instr.node.target_block() {
                self.add_edge(prev_id, target);
            };
        };

        if let Some(target) = f.blocks[f.block_order.last().unwrap()].end_instr.node.target_block() {
            self.add_edge(*f.block_order.last().unwrap(), target);
        };
    }

    pub fn pretty<'a>(&'a self, func: &'a IlFunction) -> impl Display + 'a {
        BlockFlowGraphPretty(self, func)
    }
}

struct BlockFlowGraphPretty<'a>(&'a FlowGraph<IlBlockId>, &'a IlFunction);

impl Display for BlockFlowGraphPretty<'_> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let BlockFlowGraphPretty(graph, func) = *self;

        for &id in func.block_order.iter() {
            let node = graph.get(id);

            write!(f, "{}: succ = [ ", id)?;
            for &next_id in node.edges.iter() {
                write!(f, "{} ", next_id)?;
            };
            if node.returns {
                write!(f, "(return) ")?;
            };
            write!(f, "], pred = [ ")?;
            for &prev_id in node.rev_edges.iter() {
                write!(f, "{} ", prev_id)?;
            };
            writeln!(f, "]")?;
        };

        Result::Ok(())
    }
}
