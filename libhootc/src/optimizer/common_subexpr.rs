use std::collections::HashMap;
use std::collections::hash_map::Entry;
use std::io::Write;

use smallvec::SmallVec;

use crate::bitvec::BitVec;
use crate::il::{IlFunction, IlInstructionKind, IlOperand, IlRegister};
use super::analysis::AnalysisStructures;

enum Expr {
    Neg(IlOperand),
    Not(IlOperand),
    Add(IlOperand, IlOperand),
    Sub(IlOperand, IlOperand),
    Mul(IlOperand, IlOperand),
    Eq(IlOperand, IlOperand),
    Ne(IlOperand, IlOperand)
}

impl Expr {
    pub fn from_instr(instr: &IlInstructionKind) -> Option<Expr> {
        Some(match *instr {
            IlInstructionKind::NegI32(_, ref val) => Expr::Neg(val.clone()),
            IlInstructionKind::NotI32(_, ref val) => Expr::Not(val.clone()),
            IlInstructionKind::AddI32(_, ref lhs, ref rhs) => Expr::Add(lhs.clone(), rhs.clone()),
            IlInstructionKind::SubI32(_, ref lhs, ref rhs) => Expr::Sub(lhs.clone(), rhs.clone()),
            IlInstructionKind::MulI32(_, ref lhs, ref rhs) => Expr::Mul(lhs.clone(), rhs.clone()),
            IlInstructionKind::EqI32(_, ref lhs, ref rhs) => Expr::Eq(lhs.clone(), rhs.clone()),
            IlInstructionKind::NeI32(_, ref lhs, ref rhs) => Expr::Ne(lhs.clone(), rhs.clone()),
            _ => {
                return None;
            }
        })
    }

    pub fn is_equivalent_to(&self, other: &Expr) -> bool {
        match *self {
            Expr::Not(ref self_val) => match *other {
                Expr::Not(ref other_val) => {
                    self_val == other_val
                },
                _ => false
            },
            Expr::Neg(ref self_val) => match *other {
                Expr::Neg(ref other_val) => {
                    self_val == other_val
                },
                _ => false
            },
            Expr::Add(ref self_lhs, ref self_rhs) => match *other {
                Expr::Add(ref other_lhs, ref other_rhs) => {
                    (self_lhs == other_lhs && self_rhs == other_rhs)
                        || (self_lhs == other_rhs && self_rhs == other_lhs)
                },
                _ => false
            },
            Expr::Sub(ref self_lhs, ref self_rhs) => match *other {
                Expr::Sub(ref other_lhs, ref other_rhs) => {
                    self_lhs == other_lhs && self_rhs == other_rhs
                },
                _ => false
            },
            Expr::Mul(ref self_lhs, ref self_rhs) => match *other {
                Expr::Mul(ref other_lhs, ref other_rhs) => {
                    (self_lhs == other_lhs && self_rhs == other_rhs)
                        || (self_lhs == other_rhs && self_rhs == other_lhs)
                },
                _ => false
            },
            Expr::Eq(ref self_lhs, ref self_rhs) => match *other {
                Expr::Eq(ref other_lhs, ref other_rhs) => {
                    (self_lhs == other_lhs && self_rhs == other_rhs)
                        || (self_lhs == other_rhs && self_rhs == other_lhs)
                },
                _ => false
            },
            Expr::Ne(ref self_lhs, ref self_rhs) => match *other {
                Expr::Ne(ref other_lhs, ref other_rhs) => {
                    (self_lhs == other_lhs && self_rhs == other_rhs)
                        || (self_lhs == other_rhs && self_rhs == other_lhs)
                },
                _ => false
            }
        }
    }
}

pub fn eliminate_local_common_subexpressions(
    func: &mut IlFunction, structs: &AnalysisStructures, w: &mut Write
) -> usize {
    let ebbs = &structs.ebbs;

    writeln!(w, "\n===== LOCAL COMMON SUBEXPRESSION ELIMINATION =====\n").unwrap();

    let mut num_replaced = 0;
    let mut exprs = vec![];
    let mut reg_exprs: HashMap<IlRegister, SmallVec<[usize; 2]>> = HashMap::new();

    let mut examined = BitVec::new();

    for ebb in ebbs.iter() {
        exprs.clear();
        reg_exprs.clear();

        for &id in ebb.blocks.iter() {
            let block = func.blocks.get_mut(&id).unwrap();
            let examined = examined.set(id, true);

            for (i, instr) in block.instrs.iter_mut().enumerate() {
                let target = match instr.node.target() {
                    Some(target) => target,
                    None => {
                        continue;
                    }
                };
                let expr = Expr::from_instr(&instr.node);
                let mut replaced = false;

                if !examined {
                    if let Some(ref expr) = expr {
                        for &(ref known_expr, reg) in exprs.iter() {
                            if let Some(ref known_expr) = *known_expr {
                                if expr.is_equivalent_to(known_expr) {
                                    writeln!(w, "Replacing calculation at {}:{} with {}", id, i, reg).unwrap();
                                    instr.node = IlInstructionKind::Copy(target, IlOperand::Register(reg));
                                    replaced = true;
                                    num_replaced += 1;

                                    break;
                                };
                            };
                        };
                    };
                };

                let expr = if replaced {
                    None
                } else {
                    expr
                };

                let new_expr_i = if let Some(expr) = expr {
                    exprs.push((Some(expr), target));
                    Some(exprs.len() - 1)
                } else {
                    None
                };

                if let Some(new_expr_i) = new_expr_i {
                    instr.node.for_operands(|o| if let IlOperand::Register(r) = *o {
                        match reg_exprs.entry(r) {
                            Entry::Occupied(mut e) => {
                                e.get_mut().push(new_expr_i);
                            },
                            Entry::Vacant(e) => {
                                e.insert(smallvec![new_expr_i]);
                            }
                        };
                    });
                };

                match reg_exprs.entry(target) {
                    Entry::Occupied(mut e) => {
                        for old_expr_i in e.get_mut().drain() {
                            exprs[old_expr_i].0 = None;
                        };

                        if let Some(new_expr_i) = new_expr_i {
                            e.get_mut().push(new_expr_i);
                        };
                    },
                    Entry::Vacant(e) => {
                        if let Some(new_expr_i) = new_expr_i {
                            e.insert(smallvec![new_expr_i]);
                        };
                    }
                };
            };
        };
    };

    if num_replaced != 0 {
        writeln!(w, "\n===== AFTER LOCAL COMMON SUBEXPRESSION ELIMINATION =====\n\n{}", func).unwrap();
    };

    num_replaced
}
