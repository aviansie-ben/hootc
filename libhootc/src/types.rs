use std::collections::HashMap;
use std::fmt;
use std::u32;

use crate::sym::SymId;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct TypeId(pub u32);

impl TypeId {
    pub fn for_error() -> TypeId { TypeId(0) }
    pub fn for_empty_tuple() -> TypeId { TypeId(1) }
    pub fn for_bool() -> TypeId { TypeId(2) }
    pub fn for_i32() -> TypeId { TypeId(3) }

    pub fn unknown() -> TypeId { TypeId(!0) }

    pub fn is_error(self) -> bool { self == TypeId::for_error() }
    pub fn is_unknown(self) -> bool { self.0 == !0 }
}

#[derive(Debug, Clone)]
pub enum Type {
    Error,
    Bool,
    I32,
    Tuple(Vec<TypeId>),
    Func(TypeId, Vec<TypeId>),
    FuncKnown(SymId, TypeId),
    Undecided(Vec<TypeId>)
}

pub struct PrettyType<'a>(pub TypeId, pub &'a TypeTable);

impl <'a> fmt::Display for PrettyType<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let PrettyType(id, table) = *self;

        if id.is_unknown() {
            write!(f, "<unknown type>")?;
        } else {
            match *table.find_type(id) {
                Type::Error => write!(f, "<error type>")?,
                Type::Bool => write!(f, "bool")?,
                Type::I32 => write!(f, "i32")?,
                Type::Tuple(ref types) => {
                    write!(f, "(")?;

                    match types.len() {
                        0 => {},
                        1 => {
                            write!(f, "{},", PrettyType(types[0], table))?;
                        },
                        _ => {
                            write!(f, "{}", PrettyType(types[0], table))?;
                            for t in &types[1..] {
                                write!(f, ", {}", PrettyType(*t, table))?;
                            };
                        }
                    };

                    write!(f, ")")?;
                },
                Type::Func(ret_type, ref arg_types) => {
                    write!(f, "fn (")?;

                    if !arg_types.is_empty() {
                        write!(f, "{}", PrettyType(arg_types[0], table))?;
                        for t in &arg_types[1..] {
                            write!(f, ", {}", PrettyType(*t, table))?;
                        };
                    };

                    write!(f, ") : {}", PrettyType(ret_type, table))?;
                },
                Type::FuncKnown(sym_id, sig_id) => {
                    let (ret_type, arg_types) = if let Type::Func(ret_type, ref arg_types) = *table.find_type(sig_id) {
                        (ret_type, arg_types)
                    } else {
                        unreachable!();
                    };

                    write!(f, "fn {}(", sym_id)?;

                    if !arg_types.is_empty() {
                        write!(f, "{}", PrettyType(arg_types[0], table))?;
                        for t in &arg_types[1..] {
                            write!(f, ", {}", PrettyType(*t, table))?;
                        };
                    };

                    write!(f, ") : {}", PrettyType(ret_type, table))?;
                },
                Type::Undecided(ref possible_types) => {
                    write!(f, "<any of ")?;

                    write!(f, "{}", PrettyType(possible_types[0], table))?;
                    for t in &possible_types[1..] {
                        write!(f, ", {}", PrettyType(*t, table))?;
                    };

                    write!(f, ">")?;
                }
            }
        };

        Result::Ok(())
    }
}

#[derive(Debug, Clone)]
pub struct TypeTable {
    types: Vec<Type>,
    tuple_types: HashMap<Vec<TypeId>, TypeId>,
    func_types: HashMap<(TypeId, Vec<TypeId>), TypeId>,
    undecided_types: HashMap<Vec<TypeId>, TypeId>
}

impl TypeTable {
    pub fn new() -> TypeTable {
        let mut table = TypeTable {
            types: vec![],
            tuple_types: HashMap::new(),
            func_types: HashMap::new(),
            undecided_types: HashMap::new()
        };

        assert_eq!(table.add_type(Type::Error), TypeId::for_error());
        assert_eq!(table.add_type(Type::Tuple(vec![])), TypeId::for_empty_tuple());
        assert_eq!(table.add_type(Type::Bool), TypeId::for_bool());
        assert_eq!(table.add_type(Type::I32), TypeId::for_i32());

        table
    }

    pub fn add_type(&mut self, t: Type) -> TypeId {
        self.types.push(t);
        assert!(self.types.len() < u32::MAX as usize);

        let id = TypeId((self.types.len() - 1) as u32);

        match self.types[id.0 as usize] {
            Type::Tuple(ref types) => {
                self.tuple_types.insert(types.clone(), id);
            },
            Type::Func(ret_type, ref param_types) => {
                self.func_types.insert((ret_type, param_types.clone()), id);
            },
            Type::Undecided(ref types) => {
                self.undecided_types.insert(types.clone(), id);
            },
            _ => {}
        };

        id
    }

    pub fn find_type(&self, id: TypeId) -> &Type {
        assert!(!id.is_unknown());
        &self.types[id.0 as usize]
    }

    pub fn find_type_mut(&mut self, id: TypeId) -> &mut Type {
        assert!(!id.is_unknown());
        &mut self.types[id.0 as usize]
    }

    pub fn is_type_undecided(&self, id: TypeId) -> bool {
        if id.is_unknown() {
            return true;
        };

        match self.types[id.0 as usize] {
            Type::Tuple(ref ts) => ts.iter().any(|&t| self.is_type_undecided(t)),
            Type::Undecided(_) => true,
            Type::Func(ret, ref params) => self.is_type_undecided(ret) || params.iter().any(|&p| self.is_type_undecided(p)),
            _ => false
        }
    }

    pub fn get_call_signature(&self, id: TypeId) -> Option<(TypeId, Vec<TypeId>)> {
        assert!(!id.is_unknown());
        match self.types[id.0 as usize] {
            Type::Func(ret, ref params) => Some((ret, params.clone())),
            Type::FuncKnown(_, sig_id) => self.get_call_signature(sig_id),
            _ => None
        }
    }

    pub fn can_trivially_convert(&self, from: TypeId, to: TypeId) -> bool {
        assert!(!from.is_unknown() && !to.is_unknown());

        if from == to {
            return true;
        };

        match self.types[from.0 as usize] {
            Type::Error => true,
            Type::Undecided(ref types) => types.contains(&to),
            _ => false
        }
    }

    pub fn least_upper_bound(&mut self, a: TypeId, b: TypeId) -> Option<TypeId> {
        if a.is_unknown() {
            if b.is_unknown() {
                return None;
            } else {
                return Some(b);
            };
        } else if b.is_unknown() {
            return Some(a);
        };

        if a.is_error() || b.is_error() {
            return Some(TypeId::for_error());
        } else if a == b {
            return Some(a);
        };

        let a_type = &self.types[a.0 as usize];
        let b_type = &self.types[b.0 as usize];

        match (a_type, b_type) {
            (Type::Undecided(ref a_types), Type::Undecided(ref b_types)) => {
                let types = a_types.iter().chain(b_types).cloned().collect();
                let t = self.get_or_add_undecided_type(types);

                if t.is_error() {
                    None
                } else {
                    Some(t)
                }
            },
            (Type::Undecided(ref types), _) => {
                if types.contains(&b) {
                    Some(b)
                } else {
                    None
                }
            },
            (_, Type::Undecided(ref types)) => {
                if types.contains(&a) {
                    Some(a)
                } else {
                    None
                }
            },
            _ => None
        }
    }

    pub fn get_or_add_tuple_type(&mut self, types: Vec<TypeId>) -> TypeId {
        if let Some(&id) = self.tuple_types.get(&types) {
            return id;
        };

        self.add_type(Type::Tuple(types))
    }

    pub fn get_or_add_function_type(&mut self, ret_type: TypeId, param_types: Vec<TypeId>) -> TypeId {
        let sig = (ret_type, param_types);

        if let Some(&id) = self.func_types.get(&sig) {
            return id;
        };

        self.add_type(Type::Func(sig.0, sig.1))
    }

    pub fn get_or_add_undecided_type(&mut self, mut types: Vec<TypeId>) -> TypeId {
        types.sort_by_key(|&TypeId(id)| id);
        types.dedup();

        match types.len() {
            0 => TypeId::for_error(),
            1 => types[0],
            _ => {
                if let Some(&id) = self.undecided_types.get(&types) {
                    return id;
                };

                self.add_type(Type::Undecided(types))
            }
        }
    }

    pub fn num_types(&self) -> u32 {
        self.types.len() as u32
    }
}

impl Default for TypeTable {
    fn default() -> Self {
        Self::new()
    }
}
