use ast::*;
use lex::{Lexer, Span, Token};

pub type Error = (String, Span);

macro_rules! bad_token {
    ($actual:expr, $expect:expr) => {{
        let &(ref tok, span) = $actual;
        return Result::Err((format!("expected {} but found {}", $expect, tok), span));
    }}
}

macro_rules! expect_token {
    ($lexer:expr, $expect_name:expr, $expect_pat:pat) => {{
        expect_token!($lexer, $expect_name, $expect_pat if true => {})
    }};
    ($lexer:expr, $expect_name:expr, $expect_pat:pat if $expect_cond:expr) => {{
        expect_token!($lexer, $expect_name, $expect_pat if $expect_cond => {})
    }};
    ($lexer:expr, $expect_name:expr, $expect_pat:pat => $result:expr) => {{
        expect_token!($lexer, $expect_name, $expect_pat if true => $result)
    }};
    ($lexer:expr, $expect_name:expr, $expect_pat:pat if $expect_cond:expr => $result:expr) => {{
        let tok = $lexer.pop();

        match tok {
            $expect_pat if $expect_cond => $result,
            t => bad_token!(&t, $expect_name)
        }
    }}
}

pub fn pop_ident(lexer: &mut Lexer) -> Result<Ident, Error> {
    Result::Ok(expect_token!(
        lexer,
        "an identifier",
        (Token::Id(name), span) => Ident::new(name.to_owned(), span)
    ))
}

fn parse_ty_tuple_end(lexer: &mut Lexer, types: &mut Vec<Ty>) -> Result<(), Error> {
    while let Token::Comma = lexer.peek().0 {
        lexer.pop();

        if !matches!(lexer.peek().0, Token::RPar) {
            types.push(parse_ty(lexer)?);
        };
    };

    if !matches!(lexer.peek().0, Token::RPar) {
        bad_token!(lexer.peek(), "',' or ')'");
    };

    Result::Ok(())
}

pub fn parse_ty(lexer: &mut Lexer) -> Result<Ty, Error> {
    Result::Ok(match lexer.peek().0 {
        Token::Id(_) => Ty::id(pop_ident(lexer)?),
        Token::LPar => {
            let lo = lexer.pop().1.lo;

            if let Token::RPar = lexer.peek().0 {
                Ty::tuple(vec![], Span::from(lo, lexer.pop().1.hi))
            } else {
                let ty = parse_ty(lexer)?;

                match lexer.peek().0 {
                    Token::RPar => {
                        lexer.pop();
                        ty
                    },
                    Token::Comma => {
                        let mut tys = vec![ty];

                        parse_ty_tuple_end(lexer, &mut tys)?;

                        Ty::tuple(tys, Span::from(lo, lexer.pop().1.hi))
                    },
                    _ => bad_token!(lexer.peek(), "',' or ')'")
                }
            }
        },
        _ => bad_token!(lexer.peek(), "a type")
    })
}

fn parse_atom_tuple_end(lexer: &mut Lexer, vals: &mut Vec<Expr>) -> Result<(), Error> {
    while let Token::Comma = lexer.peek().0 {
        lexer.pop();

        if !matches!(lexer.peek().0, Token::RPar) {
            vals.push(parse_expr(lexer)?);
        };
    };

    if !matches!(lexer.peek().0, Token::RPar) {
        bad_token!(lexer.peek(), "',' or ')'");
    };

    Result::Ok(())
}

fn parse_lambda_params(lexer: &mut Lexer) -> Result<(Vec<VarDecl>, Span), Error> {
    let lo = expect_token!(lexer, "'('", (Token::LPar, span) => span.lo);

    Result::Ok(if let Token::RPar = lexer.peek().0 {
        let hi = lexer.pop().1.hi;
        (vec![], Span::from(lo, hi))
    } else {
        let mut params = vec![];

        loop {
            let mutable = if let Token::Mut = lexer.peek().0 {
                lexer.pop();
                true
            } else {
                false
            };
            let id = pop_ident(lexer)?;

            let ty = match lexer.peek().0 {
                Token::Colon => {
                    lexer.pop();
                    parse_ty(lexer)?
                },
                _ => Ty::infer()
            };

            params.push(VarDecl::new(mutable, id, ty));

            match lexer.peek().0 {
                Token::Comma => {
                    lexer.pop();
                },
                Token::RPar => {
                    let hi = lexer.pop().1.hi;
                    break (params, Span::from(lo, hi));
                },
                _ => bad_token!(lexer.peek(), "':', ',', or ')'")
            }
        }
    })
}

pub fn parse_args(lexer: &mut Lexer) -> Result<Vec<Expr>, Error> {
    if matches!(lexer.peek().0, Token::RPar) {
        return Result::Ok(vec![]);
    };

    let mut args = vec![parse_expr(lexer)?];

    while let Token::Comma = lexer.peek().0 {
        lexer.pop();
        args.push(parse_expr(lexer)?);
    };

    Result::Ok(args)
}

pub fn parse_atom_suffix(mut expr: Expr, lexer: &mut Lexer) -> Result<Expr, Error> {
    loop {
        expr = match lexer.peek().0 {
            Token::LPar => {
                let lo = lexer.pop().1.lo;
                let args = parse_args(lexer)?;
                let hi = expect_token!(lexer, "')'", (Token::RPar, span) => span.hi);

                Expr::call(expr, args, Span::from(lo, hi))
            },
            _ => break
        };
    };
    Result::Ok(expr)
}

pub fn parse_unary_prefix_atom(op: UnOp, lexer: &mut Lexer) -> Result<Expr, Error> {
    let lo = lexer.pop().1.lo;
    let expr = parse_atom(lexer)?;
    let span = Span::from(lo, expr.span.hi);

    Result::Ok(Expr::un_op(op, expr, span))
}

pub fn parse_atom(lexer: &mut Lexer) -> Result<Expr, Error> {
    let expr = match lexer.peek().0 {
        Token::Id(_) => Expr::id(pop_ident(lexer)?),
        Token::Int(val) => if let Result::Ok(val) = val.parse::<u128>() {
            Expr::int(val, lexer.pop().1)
        } else {
            // TODO Better error message
            return Result::Err((format!("integer {} is out-of-range", val), lexer.peek().1))
        },
        Token::CLPar => Expr::block(parse_block(lexer)?),
        Token::LPar => {
            let lo = lexer.pop().1.lo;

            if let Token::RPar = lexer.peek().0 {
                Expr::tuple(vec![], Span::from(lo, lexer.pop().1.hi))
            } else {
                let val = parse_expr(lexer)?;

                match lexer.peek().0 {
                    Token::RPar => {
                        Expr::paren(val, Span::from(lo, lexer.pop().1.hi))
                    },
                    Token::Comma => {
                        let mut vals = vec![val];

                        parse_atom_tuple_end(lexer, &mut vals)?;

                        Expr::tuple(vals, Span::from(lo, lexer.pop().1.hi))
                    },
                    _ => bad_token!(lexer.peek(), "',' or ')'")
                }
            }
        },
        Token::If => {
            let lo = lexer.pop().1.lo;
            let cond = parse_expr(lexer)?;
            let true_block = parse_block(lexer)?;
            let false_block = if let Token::Else = lexer.peek().0 {
                lexer.pop();
                Some(parse_block(lexer)?)
            } else {
                None
            };

            let hi = false_block.as_ref().unwrap_or(&true_block).span.hi;

            Expr::if_expr(cond, true_block, false_block, Span::from(lo, hi))
        },
        Token::While => {
            let lo = lexer.pop().1.lo;
            let cond = parse_expr(lexer)?;
            let block = parse_block(lexer)?;

            let hi = block.span.hi;

            Expr::while_expr(cond, block, Span::from(lo, hi))
        },
        Token::Fn => {
            let lo = lexer.pop().1.lo;
            let (params, params_span) = parse_lambda_params(lexer)?;

            let return_ty = match lexer.peek().0 {
                Token::Colon => {
                    lexer.pop();
                    parse_ty(lexer)?
                },
                Token::CLPar => Ty::infer(),
                _ => bad_token!(lexer.peek(), "':' or '{'")
            };

            let block = parse_block(lexer)?;
            let span = Span::from(lo, block.span.hi);

            Expr::lambda(FuncSig::new(params, params_span, return_ty), block, span)
        },
        Token::True => Expr::bool(true, lexer.pop().1),
        Token::False => Expr::bool(false, lexer.pop().1),
        Token::Sub => return parse_unary_prefix_atom(UnOp::Neg, lexer),
        Token::Not => return parse_unary_prefix_atom(UnOp::Not, lexer),
        _ => bad_token!(lexer.peek(), "an expression")
    };

    parse_atom_suffix(expr, lexer)
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
enum BinOpPrec {
    Or,
    And,
    Relational,
    AddSub,
    MulDiv
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum BinOpAssoc {
    Left,
    Right
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
struct BinOpInfo(BinOpPrec, BinOpAssoc);

fn lookup_bin_op(tok: &Token) -> Option<BinOpInfo> {
    match *tok {
        Token::Eq => Some(BinOpInfo(BinOpPrec::Relational, BinOpAssoc::Left)),
        Token::Ne => Some(BinOpInfo(BinOpPrec::Relational, BinOpAssoc::Left)),
        Token::Lt => Some(BinOpInfo(BinOpPrec::Relational, BinOpAssoc::Left)),
        Token::Gt => Some(BinOpInfo(BinOpPrec::Relational, BinOpAssoc::Left)),
        Token::Le => Some(BinOpInfo(BinOpPrec::Relational, BinOpAssoc::Left)),
        Token::Ge => Some(BinOpInfo(BinOpPrec::Relational, BinOpAssoc::Left)),
        Token::Add => Some(BinOpInfo(BinOpPrec::AddSub, BinOpAssoc::Left)),
        Token::Sub => Some(BinOpInfo(BinOpPrec::AddSub, BinOpAssoc::Left)),
        Token::Mul => Some(BinOpInfo(BinOpPrec::MulDiv, BinOpAssoc::Left)),
        Token::Div => Some(BinOpInfo(BinOpPrec::MulDiv, BinOpAssoc::Left)),
        Token::Or => Some(BinOpInfo(BinOpPrec::Or, BinOpAssoc::Left)),
        Token::And => Some(BinOpInfo(BinOpPrec::And, BinOpAssoc::Left)),
        _ => None
    }
}

fn map_bin_op(tok: &Token) -> BinOp {
    match *tok {
        Token::Eq => BinOp::Eq,
        Token::Ne => BinOp::Ne,
        Token::Lt => BinOp::Lt,
        Token::Gt => BinOp::Gt,
        Token::Le => BinOp::Le,
        Token::Ge => BinOp::Ge,
        Token::Add => BinOp::Add,
        Token::Sub => BinOp::Sub,
        Token::Mul => BinOp::Mul,
        Token::Div => BinOp::Div,
        Token::Or => BinOp::Or,
        Token::And => BinOp::And,
        _ => panic!("{} is not valid as a binary operator", tok)
    }
}

pub fn parse_expr(lexer: &mut Lexer) -> Result<Expr, Error> {
    let expr = parse_atom(lexer)?;

    Result::Ok(if let Some(op_info) = lookup_bin_op(&lexer.peek().0) {
        let mut output_stack = vec![expr];
        let mut operand_stack = vec![(lexer.pop(), op_info)];

        output_stack.push(parse_atom(lexer)?);

        while let Some(op_info) = lookup_bin_op(&lexer.peek().0) {
            while let Some(&(_, top_op_info)) = operand_stack.last() {
                if op_info.0 > top_op_info.0 || (op_info.0 == top_op_info.0 && op_info.1 == BinOpAssoc::Right) {
                    break;
                };

                let right = output_stack.pop().unwrap();
                let left = output_stack.pop().unwrap();
                let op = map_bin_op(&(operand_stack.pop().unwrap().0).0);

                output_stack.push(Expr::bin_op(op, left, right));
            };

            operand_stack.push((lexer.pop(), op_info));
            output_stack.push(parse_atom(lexer)?);
        };

        while let Some(((op_tok, _), _)) = operand_stack.pop() {
            let right = output_stack.pop().unwrap();
            let left = output_stack.pop().unwrap();
            let op = map_bin_op(&op_tok);

            output_stack.push(Expr::bin_op(op, left, right));
        };

        assert_eq!(output_stack.len(), 1);
        output_stack.pop().unwrap()
    } else {
        expr
    })
}

pub fn parse_stmt(lexer: &mut Lexer) -> Result<Stmt, Error> {
    Result::Ok(match lexer.peek().0 {
        Token::Let => {
            let lo = lexer.pop().1.lo;
            let mutable = if let Token::Mut = lexer.peek().0 {
                lexer.pop();
                true
            } else {
                false
            };
            let id = pop_ident(lexer)?;

            let ty = match lexer.peek().0 {
                Token::Colon => {
                    lexer.pop();
                    let ty = parse_ty(lexer)?;

                    expect_token!(lexer, "'='", (Token::Assign, _));
                    ty
                },
                Token::Assign => {
                    lexer.pop();
                    Ty::infer()
                },
                _ => bad_token!(lexer.peek(), "':' or '='")
            };

            let val = parse_expr(lexer)?;
            let hi = val.span.hi;

            Stmt::let_stmt(VarDecl::new(mutable, id, ty), val, Span::from(lo, hi))
        },
        Token::Return => {
            let lo = lexer.pop().1.lo;
            let val = parse_expr(lexer)?;
            let hi = val.span.hi;

            Stmt::return_stmt(val, Span::from(lo, hi))
        },
        _ => {
            let expr = parse_expr(lexer)?;

            match lexer.peek().0 {
                Token::Assign => {
                    lexer.pop();
                    Stmt::assign(expr, parse_expr(lexer)?)
                },
                _ => Stmt::expr(expr)
            }
        }
    })
}

pub fn parse_block(lexer: &mut Lexer) -> Result<Block, Error> {
    let lo = expect_token!(lexer, "'{'", (Token::CLPar, span) => span.lo);
    let mut block = Block::new();

    while !matches!(*lexer.peek(), (Token::CRPar, _)) {
        let stmt = parse_stmt(lexer)?;

        match lexer.peek().0 {
            Token::Semi => {
                lexer.pop();
                block.stmts.push(stmt)
            },
            _ => if let StmtKind::Expr(expr) = stmt.node {
                block.result = Some(expr);
                break;
            } else {
                bad_token!(lexer.peek(), "';'");
            }
        };
    };

    let hi = expect_token!(lexer, "'}' or ';'", (Token::CRPar, span) => span.hi);

    block.span = Span::from(lo, hi);
    Result::Ok(block)
}

pub fn parse_func_sig_suffix(lexer: &mut Lexer) -> Result<FuncSig, Error> {
    let params_lo = expect_token!(lexer, "'('", (Token::LPar, span) => span.lo);

    let mut params = vec![];

    if !matches!(lexer.peek().0, Token::RPar) {
        loop {
            let mutable = if let Token::Mut = lexer.peek().0 {
                lexer.pop();
                true
            } else {
                false
            };
            let id = pop_ident(lexer)?;
            expect_token!(lexer, "':'", (Token::Colon, _));

            let param_ty = parse_ty(lexer)?;

            params.push(VarDecl::new(mutable, id, param_ty));

            if matches!(lexer.peek().0, Token::Comma) {
                lexer.pop()
            } else {
                break
            };
        }
    }

    let params_hi = expect_token!(lexer, "')'", (Token::RPar, span) => span.hi);
    expect_token!(lexer, "':'", (Token::Colon, _));

    let return_ty = parse_ty(lexer)?;

    Result::Ok(FuncSig::new(params, Span::from(params_lo, params_hi), return_ty))
}

pub fn parse_func_decl(lexer: &mut Lexer) -> Result<(Ident, FuncSig), Error> {
    expect_token!(lexer, "'fn'", (Token::Fn, _));
    Result::Ok((pop_ident(lexer)?, parse_func_sig_suffix(lexer)?))
}

pub fn parse_func(lexer: &mut Lexer) -> Result<Function, Error> {
    let (id, sig) = parse_func_decl(lexer)?;
    let body = parse_block(lexer)?;

    Result::Ok(Function::new(id, sig, body))
}

pub fn parse_import_part(lexer: &mut Lexer, id: Option<Ident>) -> Result<ImportPart, Error> {
    let id = id.map_or_else(|| pop_ident(lexer), Result::Ok)?;
    let as_id = if matches!(*lexer.peek(), (Token::As, _)) {
        lexer.pop();
        Some(pop_ident(lexer)?)
    } else {
        None
    };

    Result::Ok(ImportPart::new(id, as_id))
}

pub fn parse_import(lexer: &mut Lexer) -> Result<Import, Error> {
    let lo = lexer.pop().1.lo;

    let mut path_parts = vec![pop_ident(lexer)?];
    let path_lo = path_parts[0].span.lo;

    let (import_parts, hi) = loop {
        match lexer.peek().0 {
            Token::Scope => {
                lexer.pop();

                match lexer.peek().0 {
                    Token::CLPar => {
                        lexer.pop();

                        let mut import_parts = vec![parse_import_part(lexer, None)?];

                        while matches!(lexer.peek().0, Token::Comma) {
                            lexer.pop();
                            import_parts.push(parse_import_part(lexer, None)?);
                        };

                        break (import_parts, expect_token!(lexer, "'}'", (Token::CRPar, span) => span.hi));
                    },
                    _ => path_parts.push(pop_ident(lexer)?)
                };
            },
            _ => {
                let part = parse_import_part(lexer, Some(path_parts.pop().unwrap()))?;
                let hi = part.as_id.as_ref().map_or_else(|| &part.id, |id| id).span.hi;
                break (vec![part], hi)
            }
        };
    };

    expect_token!(lexer, "';'", (Token::Semi, _));

    let path = if path_parts.is_empty() {
        None
    } else {
        let span = Span::from(path_lo, path_parts.last().unwrap().span.hi);
        Some(ScopePath::new(path_parts, span))
    };

    Result::Ok(Import::new(path, import_parts, Span::from(lo, hi)))
}

pub fn parse_module(lexer: &mut Lexer) -> Result<Module, Error> {
    let mut module = Module::new();

    loop {
        match lexer.peek().0 {
            Token::Import => module.imports.push(parse_import(lexer)?),
            Token::Fn => module.funcs.push(parse_func(lexer)?),
            _ => return Result::Ok(module)
        }
    }
}
