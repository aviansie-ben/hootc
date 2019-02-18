use std::cmp::{max, min, Ordering};
use std::fmt;
use std::mem;

use plex::lexer;

#[derive(Debug, Clone)]
pub enum Token<'a> {
    LPar,
    RPar,
    CLPar,
    CRPar,

    Comma,
    Dot,
    Semi,
    Colon,
    Scope,

    Add,
    Sub,
    Mul,
    Div,
    BitAnd,
    BitOr,
    Not,
    Arrow,
    Pipeline,

    And,
    Or,

    Eq,
    Ne,
    Lt,
    Gt,
    Le,
    Ge,

    Assign,
    AssignAdd,
    AssignSub,
    AssignMul,
    AssignDiv,

    Newline,
    Whitespace(&'a str),

    Import,
    As,
    Fn,
    Let,
    Mut,
    Move,
    Return,
    If,
    Else,
    While,
    True,
    False,

    Id(&'a str),
    Int(&'a str),

    BadChar(char),
    EndOfFile
}

impl <'a> fmt::Display for Token<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            Token::LPar => write!(f, "'('"),
            Token::RPar => write!(f, "')'"),
            Token::CLPar => write!(f, "'{{'"),
            Token::CRPar => write!(f, "'}}'"),

            Token::Comma => write!(f, "','"),
            Token::Dot => write!(f, "'.'"),
            Token::Semi => write!(f, "';'"),
            Token::Colon => write!(f, "':'"),
            Token::Scope => write!(f, "'::'"),

            Token::Add => write!(f, "'+'"),
            Token::Sub => write!(f, "'-'"),
            Token::Mul => write!(f, "'*'"),
            Token::Div => write!(f, "'/'"),
            Token::BitAnd => write!(f, "'&'"),
            Token::BitOr => write!(f, "'|'"),
            Token::Not => write!(f, "'!'"),
            Token::Arrow => write!(f, "'->'"),
            Token::Pipeline => write!(f, "'|>'"),

            Token::And => write!(f, "'&&'"),
            Token::Or => write!(f, "'||'"),

            Token::Eq => write!(f, "'=='"),
            Token::Ne => write!(f, "'!='"),
            Token::Lt => write!(f, "'<'"),
            Token::Gt => write!(f, "'>'"),
            Token::Le => write!(f, "'<='"),
            Token::Ge => write!(f, "'>='"),

            Token::Assign => write!(f, "'='"),
            Token::AssignAdd => write!(f, "'+='"),
            Token::AssignSub => write!(f, "'-='"),
            Token::AssignMul => write!(f, "'*='"),
            Token::AssignDiv => write!(f, "'/='"),

            Token::Newline => write!(f, "newline"),
            Token::Whitespace(_) => write!(f, "whitespace"),

            Token::Import => write!(f, "'import'"),
            Token::As => write!(f, "'as'"),
            Token::Fn => write!(f, "'fn'"),
            Token::Let => write!(f, "'let'"),
            Token::Mut => write!(f, "'mut'"),
            Token::Move => write!(f, "'move'"),
            Token::Return => write!(f, "'return'"),
            Token::If => write!(f, "'if'"),
            Token::Else => write!(f, "'else'"),
            Token::While => write!(f, "'while'"),
            Token::True => write!(f, "'true'"),
            Token::False => write!(f, "'false'"),

            Token::Id(id) => write!(f, "identifier '{}'", id),
            Token::Int(val) => write!(f, "'{}'", val),

            Token::BadChar(ch) => write!(f, "'{}'", ch),
            Token::EndOfFile => write!(f, "end of file")
        }
    }
}

#[derive(Clone, Copy, Eq)]
pub struct Pos {
    pub line: usize,
    pub col: usize,
    pub off: usize
}

impl Pos {
    fn dummy() -> Pos {
        Pos { line: !0, col: !0, off: !0 }
    }

    fn start() -> Pos {
        Pos { line: 1, col: 1, off: 0 }
    }

    fn next_line(self, off: usize) -> Pos {
        Pos { line: self.line + 1, col: 1, off }
    }

    fn add_cols(self, cols: usize, off: usize) -> Pos {
        Pos { line: self.line, col: self.col + cols, off }
    }
}

impl fmt::Debug for Pos {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        if self.off == !0 {
            write!(f, "Pos::dummy()")
        } else {
            write!(f, "Pos(line {}, col {}, off {})", self.line, self.col, self.off)
        }
    }
}

impl PartialEq for Pos {
    fn eq(&self, other: &Pos) -> bool {
        self.off == other.off
    }
}

impl PartialOrd for Pos {
    fn partial_cmp(&self, other: &Pos) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for Pos {
    fn cmp(&self, other: &Pos) -> Ordering {
        self.off.cmp(&other.off)
    }
}

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub struct Span {
    pub lo: Pos,
    pub hi: Pos
}

impl Span {
    pub fn dummy() -> Span {
        Span { lo: Pos::dummy(), hi: Pos::dummy() }
    }

    pub fn from(a: Pos, b: Pos) -> Span {
        Span { lo: a, hi: b }
    }

    pub fn combine(a: Span, b: Span) -> Span {
        Span { lo: min(a.lo, b.lo), hi: max(a.hi, b.hi) }
    }
}

impl fmt::Debug for Span {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        if self.lo.off == !0 {
            write!(f, "Span::dummy()")
        } else {
            write!(f, "Span::from({:?}, {:?})", self.lo, self.hi)
        }
    }
}

lexer! {
    fn take_token(tok: 'a) -> (Token, &'a str);

    r#"\("# => (Token::LPar, tok),
    r#"\)"# => (Token::RPar, tok),
    r#"{"# => (Token::CLPar, tok),
    r#"}"# => (Token::CRPar, tok),

    r#","# => (Token::Comma, tok),
    r#"\."# => (Token::Dot, tok),
    r#";"# => (Token::Semi, tok),
    r#":"# => (Token::Colon, tok),
    r#"::"# => (Token::Scope, tok),

    r#"\+"# => (Token::Add, tok),
    r#"-"# => (Token::Sub, tok),
    r#"\*"# => (Token::Mul, tok),
    r#"/"# => (Token::Div, tok),
    r#"\&"# => (Token::BitAnd, tok),
    r#"\|"# => (Token::BitOr, tok),
    r#"!"# => (Token::Not, tok),
    r#"->"# => (Token::Arrow, tok),
    r#"\|>"# => (Token::Pipeline, tok),

    r#"\&\&"# => (Token::And, tok),
    r#"\|\|"# => (Token::Or, tok),

    r#"=="# => (Token::Eq, tok),
    r#"!="# => (Token::Ne, tok),
    r#"<"# => (Token::Lt, tok),
    r#">"# => (Token::Gt, tok),
    r#"<="# => (Token::Le, tok),
    r#">="# => (Token::Ge, tok),

    r#"="# => (Token::Assign, tok),
    r#"\+="# => (Token::AssignAdd, tok),
    r#"-="# => (Token::AssignSub, tok),
    r#"\*="# => (Token::AssignMul, tok),
    r#"/="# => (Token::AssignDiv, tok),

    r#"\r?\n"# => (Token::Newline, tok),
    r#"[ \t]+"# => (Token::Whitespace(tok), tok),

    r#"as"# => (Token::As, tok),
    r#"import"# => (Token::Import, tok),
    r#"fn"# => (Token::Fn, tok),
    r#"let"# => (Token::Let, tok),
    r#"mut"# => (Token::Mut, tok),
    r#"move"# => (Token::Move, tok),
    r#"return"# => (Token::Return, tok),
    r#"if"# => (Token::If, tok),
    r#"else"# => (Token::Else, tok),
    r#"while"# => (Token::While, tok),
    r#"true"# => (Token::True, tok),
    r#"false"# => (Token::False, tok),

    r#"[a-zA-Z_][a-zA-Z0-9_]*"# => (Token::Id(tok), tok),
    r#"[0-9]+"# => (Token::Int(tok), tok),

    r#"."# => (Token::BadChar(tok.chars().next().unwrap()), tok)
}

#[derive(Debug)]
pub struct LexerRestorePoint(Pos);

pub struct Lexer<'a> {
    contents: &'a str,
    cur_pos: Pos,

    next_token: (Token<'a>, Span),

    panic_mode: bool
}

impl <'a> Lexer<'a> {
    pub fn new(contents: &'a str) -> Lexer<'a> {
        let mut lexer = Lexer {
            contents,
            cur_pos: Pos::start(),

            next_token: (Token::EndOfFile, Span::dummy()),

            panic_mode: false
        };
        lexer.pop();

        lexer
    }

    fn next_token_noskip(&mut self) -> (Token<'a>, Span) {
        if let Some(((tok, tok_text), rem)) = take_token(&self.contents[self.cur_pos.off..]) {
            let lo = self.cur_pos;
            let hi_off = (rem.as_ptr() as usize) - (self.contents.as_ptr() as usize);

            let hi = match tok {
                Token::Newline => self.cur_pos.next_line(hi_off),
                _ => self.cur_pos.add_cols(tok_text.chars().count(), hi_off)
            };
            self.cur_pos = hi;

            (tok, Span::from(lo, hi))
        } else {
            (Token::EndOfFile, Span::from(self.cur_pos, self.cur_pos))
        }
    }

    pub fn pop(&mut self) -> (Token<'a>, Span) {
        let next_token = loop {
            let (tok, tok_span) = self.next_token_noskip();

            match tok {
                Token::BadChar(_) if self.panic_mode => continue,
                Token::BadChar(_) => self.panic_mode = true,
                _ => self.panic_mode = false
            };

            match tok {
                Token::Newline | Token::Whitespace(_) => continue,
                _ => break (tok, tok_span)
            };
        };

        mem::replace(&mut self.next_token, next_token)
    }

    pub fn peek(&self) -> &(Token<'a>, Span) {
        &self.next_token
    }

    pub fn save(&self) -> LexerRestorePoint {
        LexerRestorePoint(self.next_token.1.lo)
    }

    pub fn restore(&mut self, rp: LexerRestorePoint) {
        self.cur_pos = rp.0;
        self.pop();
    }
}
