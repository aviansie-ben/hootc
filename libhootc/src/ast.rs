use std::collections::HashMap;
use std::fmt;

use lex::Span;
use sym::{SymDefTable, SymId};
use types::{PrettyType, TypeId, TypeTable};

#[derive(Debug, Clone)]
pub struct Module {
    pub imports: Vec<Import>,
    pub funcs: Vec<Function>,
    pub lifted: Vec<Function>,
    pub exports: HashMap<String, SymId>,
    pub types: TypeTable,
    pub syms: SymDefTable
}

impl Module {
    pub fn new() -> Module {
        Module {
            imports: vec![],
            funcs: vec![],
            lifted: vec![],
            exports: HashMap::new(),
            types: TypeTable::new(),
            syms: SymDefTable::new()
        }
    }
}

impl Default for Module {
    fn default() -> Self {
        Self::new()
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ScopePath {
    pub parts: Vec<Ident>,
    pub span: Span
}

impl ScopePath {
    pub fn new(parts: Vec<Ident>, span: Span) -> ScopePath {
        ScopePath { parts, span }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Import {
    pub path: Option<ScopePath>,
    pub parts: Vec<ImportPart>,
    pub span: Span
}

impl Import {
    pub fn new(path: Option<ScopePath>, parts: Vec<ImportPart>, span: Span) -> Import {
        Import { path, parts, span }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ImportPart {
    pub id: Ident,
    pub as_id: Option<Ident>
}

impl ImportPart {
    pub fn new(id: Ident, as_id: Option<Ident>) -> ImportPart {
        ImportPart { id, as_id }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TyKind {
    Ident(String),
    Tuple(Vec<Ty>),
    Ref(Box<Ty>),
    Infer
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Ty {
    pub node: TyKind,
    pub span: Span,
    pub type_id: TypeId
}

impl Ty {
    pub fn id(id: Ident) -> Ty {
        Ty {
            node: TyKind::Ident(id.id),
            span: id.span,
            type_id: TypeId::unknown()
        }
    }

    pub fn tuple(elems: Vec<Ty>, span: Span) -> Ty {
        Ty {
            node: TyKind::Tuple(elems),
            span,
            type_id: TypeId::unknown()
        }
    }

    pub fn infer() -> Ty {
        Ty {
            node: TyKind::Infer,
            span: Span::dummy(),
            type_id: TypeId::unknown()
        }
    }

    pub fn is_infer(&self) -> bool {
        match self.node {
            TyKind::Infer => true,
            _ => false
        }
    }
}

struct PrettyTy<'a>(&'a Ty, &'a str, &'a TypeTable, u32);

impl <'a> fmt::Display for PrettyTy<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use self::TyKind::*;

        let PrettyTy(t, indent, types, max_levels) = *self;
        let more_indent = format!("  {}", indent);

        let next_indent = &more_indent[1..];

        match t.node {
            Ident(ref id) => write!(f, "TyId {}", id),
            Tuple(_) => write!(f, "Tuple"),
            Ref(_) => write!(f, "Ref"),
            Infer => write!(f, "InferTy")
        }?;

        if t.span != Span::dummy() {
            let Span { lo, hi } = t.span;
            write!(f, " [span: {}:{} - {}:{}]", lo.line, lo.col, hi.line, hi.col)?;
        };

        if !t.type_id.is_unknown() {
            write!(f, " [type: {}]", PrettyType(t.type_id, types))?;
        };

        match t.node {
            Ident(_) => {},
            Tuple(ref tys) => {
                if max_levels > 0 {
                    for ty in tys {
                        write!(f, "\n{}{}", next_indent, PrettyTy(ty, next_indent, types, max_levels - 1))?;
                    };
                } else {
                    write!(f, "\n{}...", next_indent)?;
                };
            },
            Ref(ref ty) => {
                if max_levels > 0 {
                    write!(f, "\n{}{}", next_indent, PrettyTy(ty, next_indent, types, max_levels - 1))?;
                } else {
                    write!(f, "\n{}...", next_indent)?;
                };
            },
            Infer => {}
        };

        Result::Ok(())
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct VarDecl {
    pub mutable: bool,
    pub id: Ident,
    pub ty: Ty
}

impl VarDecl {
    pub fn new(mutable: bool, id: Ident, ty: Ty) -> VarDecl {
        VarDecl { mutable, id, ty }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct FuncSig {
    pub params: Vec<VarDecl>,
    pub params_span: Span,
    pub return_type: Ty
}

impl FuncSig {
    pub fn new(params: Vec<VarDecl>, params_span: Span, return_type: Ty) -> FuncSig {
        FuncSig { params, params_span, return_type }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Function {
    pub name: Ident,
    pub sig: FuncSig,
    pub body: Block,
    pub sym_id: Option<SymId>,
    pub id: u32
}

impl Function {
    pub fn new(name: Ident, sig: FuncSig, body: Block) -> Function {
        Function { name, sig, body, sym_id: None, id: !0 }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Block {
    pub stmts: Vec<Stmt>,
    pub result: Option<Expr>,
    pub span: Span
}

impl Block {
    pub fn new() -> Block {
        Block { stmts: vec![], result: None, span: Span::dummy() }
    }

    pub fn from_expr(expr: Expr) -> Block {
        let span = expr.span;
        Block { stmts: vec![], result: Some(expr), span }
    }
}

impl Default for Block {
    fn default() -> Self {
        Self::new()
    }
}

struct PrettyBlock<'a>(&'a Block, &'a str, &'a TypeTable, u32);

impl <'a> fmt::Display for PrettyBlock<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let PrettyBlock(b, indent, types, max_levels) = *self;
        let more_indent = format!("  {}", indent);

        let next_indent = &more_indent[1..];
        let next_next_indent = &more_indent[..];

        write!(f, "Block [addr: {:p}]", b)?;

        if b.span != Span::dummy() {
            let Span { lo, hi } = b.span;
            write!(f, " [span: {}:{} - {}:{}]", lo.line, lo.col, hi.line, hi.col)?;
        };

        if max_levels > 0 {
            for s in b.stmts.iter() {
                write!(f, "\n{}{}",
                    next_indent, PrettyStmt(s, next_indent, types, max_levels - 1)
                )?;
            };

            if let Some(ref result) = b.result {
                if max_levels > 1 {
                    write!(f, "\n{}Result\n{}{}",
                        next_indent,
                        next_next_indent, PrettyExpr(result, next_next_indent, types, max_levels - 2)
                    )?;
                } else {
                    write!(f, "\n{}Result\n{}...", next_indent, next_next_indent)?;
                }
            };
        } else if !b.stmts.is_empty() || b.result.is_some() {
            write!(f, "\n{}...", next_indent)?;
        };

        Result::Ok(())
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum BinOp {
    Add,
    Sub,
    Mul,
    Div,
    And,
    Or,
    Eq,
    Ne,
    Lt,
    Gt,
    Le,
    Ge
}

impl fmt::Display for BinOp {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use self::BinOp::*;
        match *self {
            Add => write!(f, "+"),
            Sub => write!(f, "-"),
            Mul => write!(f, "*"),
            Div => write!(f, "/"),
            And => write!(f, "&&"),
            Or => write!(f, "||"),
            Eq => write!(f, "=="),
            Ne => write!(f, "!="),
            Lt => write!(f, "<"),
            Gt => write!(f, ">"),
            Le => write!(f, "<="),
            Ge => write!(f, ">=")
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum UnOp {
    Neg,
    Not
}

impl fmt::Display for UnOp {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use self::UnOp::*;
        match *self {
            Neg => write!(f, "-"),
            Not => write!(f, "!")
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Ident {
    pub id: String,
    pub span: Span,
    pub sym_id: Option<SymId>
}

impl Ident {
    pub fn new(id: String, span: Span) -> Ident {
        Ident { id, span, sym_id: None }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Assignability {
    Assignable,
    NotAssignable,
    Immutable(SymId)
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum LambdaBody {
    Inline(Box<Function>),
    Lifted(SymId)
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ExprKind {
    Call(Box<Expr>, Vec<Expr>),
    BinOp(BinOp, Box<(Expr, Expr)>, Option<SymId>),
    UnOp(UnOp, Box<Expr>, Option<SymId>),
    Id(Ident),
    Block(Box<Block>),
    Tuple(Vec<Expr>),
    Paren(Box<Expr>),
    If(Box<(Expr, Block, Option<Block>)>),
    While(Box<(Expr, Block)>),
    Lambda(LambdaBody),
    Int(u128),
    Bool(bool)
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Expr {
    pub node: ExprKind,
    pub span: Span,
    pub ty: TypeId,
    pub assignable: Assignability
}

impl Expr {
    pub fn new(node: ExprKind, span: Span) -> Expr {
        Expr { node, span, ty: TypeId::unknown(), assignable: Assignability::NotAssignable }
    }

    pub fn id(id: Ident) -> Expr {
        let span = id.span;
        Expr::new(ExprKind::Id(id), span)
    }

    pub fn call(func: Expr, args: Vec<Expr>, span: Span) -> Expr {
        Expr::new(ExprKind::Call(Box::new(func), args), span)
    }

    pub fn bin_op(op: BinOp, left: Expr, right: Expr) -> Expr {
        let span = Span::from(left.span.lo, right.span.hi);
        Expr::new(ExprKind::BinOp(op, Box::new((left, right)), None), span)
    }

    pub fn un_op(op: UnOp, val: Expr, span: Span) -> Expr {
        Expr::new(ExprKind::UnOp(op, Box::new(val), None), span)
    }

    pub fn int(val: u128, span: Span) -> Expr {
        Expr::new(ExprKind::Int(val), span)
    }

    pub fn bool(val: bool, span: Span) -> Expr {
        Expr::new(ExprKind::Bool(val), span)
    }

    pub fn block(block: Block) -> Expr {
        let span = block.span;
        Expr::new(ExprKind::Block(Box::new(block)), span)
    }

    pub fn tuple(vals: Vec<Expr>, span: Span) -> Expr {
        Expr::new(ExprKind::Tuple(vals), span)
    }

    pub fn paren(val: Expr, span: Span) -> Expr {
        Expr::new(ExprKind::Paren(Box::new(val)), span)
    }

    pub fn if_expr(cond: Expr, true_block: Block, false_block: Option<Block>, span: Span) -> Expr {
        Expr::new(ExprKind::If(Box::new((cond, true_block, false_block))), span)
    }

    pub fn while_expr(cond: Expr, block: Block, span: Span) -> Expr {
        Expr::new(ExprKind::While(Box::new((cond, block))), span)
    }

    pub fn lambda(sig: FuncSig, body: Block, span: Span) -> Expr {
        Expr::new(
            ExprKind::Lambda(LambdaBody::Inline(
                Box::new(Function::new(Ident::new("<lambda>".into(), span), sig, body))
            )),
            span
        )
    }

    pub fn wrap_in_block(self) -> Block {
        match self {
            Expr { node: ExprKind::Block(block), .. } => *block,
            expr => Block::from_expr(expr)
        }
    }

    pub fn pretty<'a>(&'a self, types: &'a TypeTable, max_levels: u32) -> impl fmt::Display + 'a {
        PrettyExpr(self, "", types, max_levels)
    }
}

struct PrettyExpr<'a>(&'a Expr, &'a str, &'a TypeTable, u32);

impl <'a> fmt::Display for PrettyExpr<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use self::ExprKind::*;

        let PrettyExpr(e, indent, types, max_levels) = *self;
        let more_indent = format!("  {}", indent);

        let next_indent = &more_indent[1..];
        let next_next_indent = &more_indent[..];

        match e.node {
            Call(_, _) => write!(f, "Call"),
            BinOp(op, _, _) => write!(f, "BinOp {}", op),
            UnOp(op, _, _) => write!(f, "UnOp {}", op),
            Id(ref id) => write!(f, "Id {}", id.id),
            Block(_) => write!(f, "Block"),
            Tuple(_) => write!(f, "Tuple"),
            Paren(_) => write!(f, "Paren"),
            If(_) => write!(f, "If"),
            While(_) => write!(f, "While"),
            Lambda(_) => write!(f, "Lambda"),
            Int(val) => write!(f, "Int {}", val),
            Bool(val) => write!(f, "Bool {}", val)
        }?;

        write!(f, " [addr: {:p}]", e)?;

        if e.span != Span::dummy() {
            let Span { lo, hi } = e.span;
            write!(f, " [span: {}:{} - {}:{}]", lo.line, lo.col, hi.line, hi.col)?;
        };

        if !e.ty.is_unknown() {
            write!(f, " [type: {}]", PrettyType(e.ty, types))?;
        };

        match e.assignable {
            Assignability::Assignable => {
                write!(f, " [assignable]")?;
            },
            Assignability::NotAssignable => {},
            Assignability::Immutable(sym) => {
                write!(f, " [immutable {}]", sym)?;
            }
        };

        match e.node {
            Call(box ref func, ref args) => {
                if max_levels > 0 {
                    write!(f, "\n{}{}",
                        next_indent, PrettyExpr(func, next_indent, types, max_levels - 1)
                    )?;

                    for a in args.iter() {
                        write!(f, "\n{}{}",
                            next_indent, PrettyExpr(a, next_indent, types, max_levels - 1)
                        )?;
                    };
                } else {
                    write!(f, "\n{}...", next_indent)?;
                }
            },
            BinOp(_, box (ref lhs, ref rhs), sym) => {
                if let Some(sym) = sym {
                    write!(f, " [sym: {}]", sym)?;
                };

                if max_levels > 0 {
                    write!(f, "\n{}{}\n{}{}",
                        next_indent, PrettyExpr(lhs, next_indent, types, max_levels - 1),
                        next_indent, PrettyExpr(rhs, next_indent, types, max_levels - 1)
                    )?;
                } else {
                    write!(f, "\n{}...", next_indent)?;
                };
            },
            UnOp(_, box ref val, sym) => {
                if let Some(sym) = sym {
                    write!(f, " [sym: {}]", sym)?;
                };

                if max_levels > 0 {
                    write!(f, "\n{}{}",
                        next_indent, PrettyExpr(val, next_indent, types, max_levels - 1)
                    )?;
                } else {
                    write!(f, "\n{}...", next_indent)?;
                };
            },
            Id(ref id) => {
                if let Some(sym) = id.sym_id {
                    write!(f, " [sym: {}]", sym)?;
                };
            },
            Block(box ref block) => {
                if max_levels > 0 {
                    for s in block.stmts.iter() {
                        write!(f, "\n{}{}",
                            next_indent, PrettyStmt(s, next_indent, types, max_levels - 1)
                        )?;
                    };

                    if let Some(ref result) = block.result {
                        if max_levels > 1 {
                            write!(f, "\n{}Result\n{}{}",
                                next_indent,
                                next_next_indent, PrettyExpr(result, next_next_indent, types, max_levels - 2)
                            )?;
                        } else {
                            write!(f, "\n{}Result\n{}...", next_indent, next_next_indent)?;
                        };
                    };
                } else if !block.stmts.is_empty() || block.result.is_some() {
                    write!(f, "\n{}...", next_indent)?;
                };
            },
            Tuple(ref vals) => {
                if max_levels > 0 {
                    for v in vals.iter() {
                        write!(f, "\n{}{}",
                            next_indent, PrettyExpr(v, next_indent, types, max_levels - 1)
                        )?;
                    };
                } else if !vals.is_empty() {
                    write!(f, "\n{}...", next_indent)?;
                };
            },
            Paren(box ref val) => {
                if max_levels > 0 {
                    write!(f, "\n{}{}",
                        next_indent, PrettyExpr(val, next_indent, types, max_levels - 1)
                    )?;
                } else {
                    write!(f, "\n{}...", next_indent)?;
                };
            },
            If(box (ref cond, ref true_block, ref false_block)) => {
                if max_levels > 0 {
                    write!(f, "\n{}{}\n{}{}",
                        next_indent, PrettyExpr(cond, next_indent, types, max_levels - 1),
                        next_indent, PrettyBlock(true_block, next_indent, types, max_levels - 1)
                    )?;

                    if let Some(false_block) = false_block {
                        write!(f, "\n{}{}",
                            next_indent, PrettyBlock(false_block, next_indent, types, max_levels - 1)
                        )?;
                    };
                } else {
                    write!(f, "\n{}...", next_indent)?;
                };
            },
            While(box (ref cond, ref block)) => {
                if max_levels > 0 {
                    write!(f, "\n{}{}\n{}{}",
                        next_indent, PrettyExpr(cond, next_indent, types, max_levels - 1),
                        next_indent, PrettyBlock(block, next_indent, types, max_levels - 1)
                    )?;
                } else {
                    write!(f, "\n{}...", next_indent)?;
                };
            },
            Lambda(ref body) => {
                match *body {
                    LambdaBody::Inline(_) => {
                        write!(f, "\n{}...", next_indent)?;
                    },
                    LambdaBody::Lifted(sym_id) => {
                        write!(f, " [sym: {}]", sym_id)?;
                    }
                };
            },
            Int(_) => {},
            Bool(_) => {}
        };

        Result::Ok(())
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum StmtKind {
    Let(VarDecl, Expr),
    Return(Expr),
    Assign(Expr, Expr),
    Expr(Expr)
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Stmt {
    pub node: StmtKind,
    pub span: Span
}

impl Stmt {
    pub fn let_stmt(decl: VarDecl, val: Expr, span: Span) -> Stmt {
        Stmt { node: StmtKind::Let(decl, val), span }
    }

    pub fn return_stmt(val: Expr, span: Span) -> Stmt {
        Stmt { node: StmtKind::Return(val), span }
    }

    pub fn assign(lhs: Expr, rhs: Expr) -> Stmt {
        let span = Span::combine(lhs.span, rhs.span);
        Stmt { node: StmtKind::Assign(lhs, rhs), span }
    }

    pub fn expr(expr: Expr) -> Stmt {
        let span = expr.span;
        Stmt { node: StmtKind::Expr(expr), span }
    }

    pub fn pretty<'a>(&'a self, types: &'a TypeTable, max_levels: u32) -> impl fmt::Display + 'a {
        PrettyStmt(self, "", types, max_levels)
    }
}

struct PrettyStmt<'a>(&'a Stmt, &'a str, &'a TypeTable, u32);

impl <'a> fmt::Display for PrettyStmt<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use self::StmtKind::*;

        let PrettyStmt(stmt, indent, types, max_levels) = *self;
        let more_indent = format!("  {}", indent);

        let next_indent = &more_indent[1..];

        match stmt.node {
            Let(ref decl, _) => if decl.mutable {
                write!(f, "Let mut {}", decl.id.id)
            } else {
                write!(f, "Let {}", decl.id.id)
            },
            Return(_) => write!(f, "Return"),
            Assign(_, _) => write!(f, "Assign"),
            Expr(_) => write!(f, "Expr")
        }?;

        write!(f, " [addr: {:p}]", stmt)?;

        if stmt.span != Span::dummy() {
            let Span { lo, hi } = stmt.span;
            write!(f, " [span: {}:{} - {}:{}]", lo.line, lo.col, hi.line, hi.col)?;
        };

        match stmt.node {
            Let(ref decl, ref val) => {
                if max_levels > 0 {
                    write!(f, "\n{}{}\n{}{}",
                        next_indent, PrettyTy(&decl.ty, next_indent, types, max_levels - 1),
                        next_indent, PrettyExpr(val, next_indent, types, max_levels - 1)
                    )?;
                } else {
                    write!(f, "\n{}...", next_indent)?;
                };
            },
            Return(ref val) => {
                if max_levels > 0 {
                    write!(f, "\n{}{}",
                        next_indent, PrettyExpr(val, next_indent, types, max_levels - 1)
                    )?;
                } else {
                    write!(f, "\n{}...", next_indent)?;
                };
            },
            Assign(ref lhs, ref rhs) => {
                if max_levels > 0 {
                    write!(f, "\n{}{}\n{}{}",
                        next_indent, PrettyExpr(lhs, next_indent, types, max_levels - 1),
                        next_indent, PrettyExpr(rhs, next_indent, types, max_levels - 1)
                    )?;
                } else {
                    write!(f, "\n{}...", next_indent)?;
                };
            },
            Expr(ref val) => {
                if max_levels > 0 {
                    write!(f, "\n{}{}",
                        next_indent, PrettyExpr(val, next_indent, types, max_levels - 1)
                    )?;
                } else {
                    write!(f, "\n{}...", next_indent)?;
                };
            }
        };

        Result::Ok(())
    }
}
