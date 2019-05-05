use std::collections::HashMap;
use std::fmt;
use std::u32;

use ast::{BinOp, Ident, UnOp};
use lex::Span;
use types::{Type, TypeId, TypeTable};

lazy_static! {
    pub static ref BUILTIN_BINARY_OPS: HashMap<BinOp, Vec<(FunctionId, [TypeId; 2], TypeId)>> = {
        let i32_t = TypeId::for_i32();
        let bool_t = TypeId::for_bool();

        let mut m = HashMap::new();

        m.insert(BinOp::Add, vec![
            (FunctionId::AddI32, [i32_t, i32_t], i32_t)
        ]);
        m.insert(BinOp::Sub, vec![
            (FunctionId::SubI32, [i32_t, i32_t], i32_t)
        ]);
        m.insert(BinOp::Mul, vec![
            (FunctionId::MulI32, [i32_t, i32_t], i32_t)
        ]);
        m.insert(BinOp::And, vec![
            (FunctionId::AndBool, [bool_t, bool_t], bool_t)
        ]);
        m.insert(BinOp::Or, vec![
            (FunctionId::OrBool, [bool_t, bool_t], bool_t)
        ]);
        m.insert(BinOp::Eq, vec![
            (FunctionId::EqI32, [i32_t, i32_t], bool_t),
            (FunctionId::EqBool, [bool_t, bool_t], bool_t)
        ]);
        m.insert(BinOp::Ne, vec![
            (FunctionId::NeI32, [i32_t, i32_t], bool_t),
            (FunctionId::NeBool, [bool_t, bool_t], bool_t)
        ]);

        m
    };

    pub static ref BUILTIN_UNARY_OPS: HashMap<UnOp, Vec<(FunctionId, [TypeId; 1], TypeId)>> = {
        let i32_t = TypeId::for_i32();
        let bool_t = TypeId::for_bool();

        let mut m = HashMap::new();

        m.insert(UnOp::Neg, vec![
            (FunctionId::NegI32, [i32_t], i32_t)
        ]);
        m.insert(UnOp::Not, vec![
            (FunctionId::NotBool, [bool_t], bool_t),
            (FunctionId::NotI32, [i32_t], i32_t)
        ]);

        m
    };

    static ref BUILTIN_SIGNATURES: HashMap<FunctionId, (TypeId, Vec<TypeId>)> = {
        let void_t = TypeId::for_empty_tuple();
        let i32_t = TypeId::for_i32();
        let bool_t = TypeId::for_bool();

        let mut m = HashMap::new();

        m.insert(FunctionId::PrintI32, (void_t, vec![i32_t]));
        m.insert(FunctionId::AddI32, (i32_t, vec![i32_t, i32_t]));
        m.insert(FunctionId::SubI32, (i32_t, vec![i32_t, i32_t]));
        m.insert(FunctionId::MulI32, (i32_t, vec![i32_t, i32_t]));
        m.insert(FunctionId::EqI32, (bool_t, vec![i32_t, i32_t]));
        m.insert(FunctionId::NeI32, (bool_t, vec![i32_t, i32_t]));
        m.insert(FunctionId::NegI32, (i32_t, vec![i32_t]));
        m.insert(FunctionId::NotI32, (i32_t, vec![i32_t]));
        m.insert(FunctionId::AndBool, (bool_t, vec![bool_t, bool_t]));
        m.insert(FunctionId::OrBool, (bool_t, vec![bool_t, bool_t]));
        m.insert(FunctionId::EqBool, (bool_t, vec![bool_t, bool_t]));
        m.insert(FunctionId::NeBool, (bool_t, vec![bool_t, bool_t]));
        m.insert(FunctionId::NotBool, (bool_t, vec![bool_t]));

        m
    };
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct SymId(pub u32);

impl fmt::Display for SymId {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "#{}", self.0)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum FunctionId {
    UserDefined(SymId),
    External(String),
    PrintI32,
    AddI32,
    SubI32,
    MulI32,
    EqI32,
    NeI32,
    NegI32,
    NotI32,
    AndBool,
    OrBool,
    EqBool,
    NeBool,
    NotBool
}

impl FunctionId {
    pub fn builtin_name(&self) -> Option<&'static str> {
        use self::FunctionId::*;
        match *self {
            UserDefined(_) => None,
            External(_) => None,
            PrintI32 => Some("$__builtin_print_i32"),
            AddI32 => Some("$__builtin_add_i32"),
            SubI32 => Some("$__builtin_sub_i32"),
            MulI32 => Some("$__builtin_mul_i32"),
            EqI32 => Some("$__builtin_eq_i32"),
            NeI32 => Some("$__builtin_ne_i32"),
            NegI32 => Some("$__builtin_neg_i32"),
            NotI32 => Some("$__builtin_not_i32"),
            AndBool => Some("$__builtin_and_bool"),
            OrBool => Some("$__builtin_or_bool"),
            EqBool => Some("$__builtin_eq_bool"),
            NeBool => Some("$__builtin_ne_bool"),
            NotBool => Some("$__builtin_not_bool")
        }
    }

    pub fn builtin_sig(&self) -> Option<&'static (TypeId, Vec<TypeId>)> {
        BUILTIN_SIGNATURES.get(self)
    }
}

impl fmt::Display for FunctionId {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use self::FunctionId::*;
        match *self {
            UserDefined(sym_id) => write!(f, "<func {}>", sym_id),
            External(ref name) => write!(f, "<extern func {}>", name),
            PrintI32 => write!(f, "<builtin print(i32)>"),
            AddI32 => write!(f, "<builtin +(i32, i32)>"),
            SubI32 => write!(f, "<builtin -(i32, i32)>"),
            MulI32 => write!(f, "<builtin *(i32, i32)>"),
            EqI32 => write!(f, "<builtin ==(i32, i32)>"),
            NeI32 => write!(f, "<builtin !=(i32, i32)>"),
            NegI32 => write!(f, "<builtin -(i32)>"),
            NotI32 => write!(f, "<builtin !(i32)>"),
            AndBool => write!(f, "<builtin &&(bool, bool)>"),
            OrBool => write!(f, "<builtin ||(bool, bool)>"),
            EqBool => write!(f, "<builtin ==(bool, bool)>"),
            NeBool => write!(f, "<builtin !=(bool, bool)>"),
            NotBool => write!(f, "<builtin !(bool)>")
        }
    }
}

#[derive(Debug, Clone)]
pub enum SymDefKind {
    Function(FunctionId),
    Local,
    Param(u32)
}

impl fmt::Display for SymDefKind {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use self::SymDefKind::*;
        match *self {
            Function(ref id) => write!(f, "{}", id),
            Local => write!(f, "<local>"),
            Param(i) => write!(f, "<param {}>", i)
        }
    }
}

#[derive(Debug, Clone)]
pub struct SymDef {
    pub name: String,
    pub node: SymDefKind,
    pub ty: TypeId,
    pub span: Span,
    pub fn_sym: Option<SymId>,
    pub mutable: bool
}

impl SymDef {
    pub fn func(id: &Ident, ty: TypeId, func: FunctionId, parent_fn_sym: Option<SymId>) -> SymDef {
        SymDef {
            name: id.id.clone(),
            node: SymDefKind::Function(func),
            ty,
            span: id.span,
            fn_sym: parent_fn_sym,
            mutable: false
        }
    }

    pub fn local(id: &Ident, ty: TypeId, fn_sym: SymId, mutable: bool) -> SymDef {
        SymDef {
            name: id.id.clone(),
            node: SymDefKind::Local,
            ty,
            span: id.span,
            fn_sym: Some(fn_sym),
            mutable
        }
    }

    pub fn param(id: &Ident, ty: TypeId, param: u32, fn_sym: SymId, mutable: bool) -> SymDef {
        SymDef {
            name: id.id.clone(),
            node: SymDefKind::Param(param),
            ty,
            span: id.span,
            fn_sym: Some(fn_sym),
            mutable
        }
    }
}

#[derive(Debug, Clone)]
pub struct SymDefTable {
    defs: Vec<SymDef>,
    builtins: HashMap<FunctionId, SymId>
}

impl SymDefTable {
    pub fn new() -> SymDefTable {
        SymDefTable { defs: vec![], builtins: HashMap::new() }
    }

    pub fn find_builtin(&mut self, f: &FunctionId, t: &mut TypeTable) -> SymId {
        if let Some(&id) = self.builtins.get(f) {
            id
        } else {
            let &(ret, ref params) = f.builtin_sig().unwrap();
            let ft = t.get_or_add_function_type(
                ret,
                params.clone()
            );
            let id = self.add_known_function_symbol(SymDef::func(
                &Ident::new(f.builtin_name().unwrap().to_owned(), Span::dummy()),
                ft,
                f.clone(),
                None
            ), t);

            self.builtins.insert(f.clone(), id);
            id
        }
    }

    pub fn find(&self, id: SymId) -> &SymDef {
        &self.defs[id.0 as usize]
    }

    pub fn find_mut(&mut self, id: SymId) -> &mut SymDef {
        &mut self.defs[id.0 as usize]
    }

    pub fn add_symbol(&mut self, def: SymDef) -> SymId {
        assert!(self.defs.len() < u32::MAX as usize);
        let id = SymId(self.defs.len() as u32);

        self.defs.push(def);

        id
    }

    pub fn add_known_function_symbol(&mut self, mut def: SymDef, t: &mut TypeTable) -> SymId {
        assert!(self.defs.len() < u32::MAX as usize);
        let id = SymId(self.defs.len() as u32);

        if let SymDefKind::Function(FunctionId::UserDefined(ref mut fn_sym_id)) = def.node {
            *fn_sym_id = id;
        };
        if !t.is_type_undecided(def.ty) {
            def.ty = t.add_type(Type::FuncKnown(id, def.ty));
        };
        self.defs.push(def);

        id
    }

    pub fn narrow_known_function_type(&mut self, id: SymId, sig: TypeId, t: &mut TypeTable) -> TypeId {
        let def = self.find_mut(id);

        match *t.find_type(def.ty) {
            Type::FuncKnown(_, prev_sig) => {
                assert_eq!(sig, prev_sig);
                return def.ty;
            },
            Type::Func(_, _) => {},
            _ => unreachable!()
        };

        def.ty = if t.is_type_undecided(sig) {
            sig
        } else {
            t.add_type(Type::FuncKnown(id, sig))
        };

        def.ty
    }

    pub fn all_symbols(&self) -> impl Iterator<Item=(SymId, &SymDef)> {
        self.defs.iter().enumerate().map(|(i, def)| (SymId(i as u32), def))
    }
}

impl Default for SymDefTable {
    fn default() -> Self {
        Self::new()
    }
}

#[derive(Debug, Clone)]
pub struct SymRefTable {
    refs: HashMap<String, SymId>,
    all_refs: Vec<SymId>,
    types: HashMap<String, TypeId>
}

impl SymRefTable {
    pub fn new() -> SymRefTable {
        SymRefTable { refs: HashMap::new(), all_refs: vec![], types: HashMap::new() }
    }

    pub fn find(&self, name: &str) -> Option<SymId> {
        self.refs.get(name).cloned()
    }

    pub fn add(&mut self, name: String, id: SymId) {
        self.refs.insert(name, id);
        self.all_refs.push(id);
    }

    pub fn find_type(&self, name: &str) -> Option<TypeId> {
        self.types.get(name).cloned()
    }

    pub fn add_type(&mut self, name: String, id: TypeId) {
        self.types.insert(name, id);
    }
}

impl Default for SymRefTable {
    fn default() -> Self {
        Self::new()
    }
}

#[derive(Debug, Clone)]
pub struct ScopedSymRefTable {
    tables: Vec<SymRefTable>
}

impl ScopedSymRefTable {
    pub fn new() -> ScopedSymRefTable {
        ScopedSymRefTable { tables: vec![] }
    }

    pub fn push_scope(&mut self) {
        self.tables.push(SymRefTable::new());
    }

    pub fn push_existing_scope(&mut self, table: SymRefTable) {
        self.tables.push(table);
    }

    pub fn pop_scope(&mut self) -> SymRefTable {
        self.tables.pop().unwrap()
    }

    pub fn top(&self) -> &SymRefTable {
        self.tables.last().unwrap()
    }

    pub fn top_mut(&mut self) -> &mut SymRefTable {
        self.tables.last_mut().unwrap()
    }

    pub fn find(&self, name: &str) -> Option<SymId> {
        for table in self.tables.iter().rev() {
            if let Some(sym) = table.find(name) {
                return Some(sym);
            };
        };
        None
    }

    pub fn find_type(&self, name: &str) -> Option<TypeId> {
        for table in self.tables.iter().rev() {
            if let Some(t) = table.find_type(name) {
                return Some(t);
            };
        };
        None
    }

    pub fn is_empty(&self) -> bool {
        self.tables.is_empty()
    }
}

impl Default for ScopedSymRefTable {
    fn default() -> Self {
        Self::new()
    }
}
