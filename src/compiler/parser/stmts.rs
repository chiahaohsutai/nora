use std::fmt;

use super::super::tokenizer::Token;
use super::{ParseResult, ParserState, blocks, decls, expected, exprs};

enum ForInit {
    Decl(decls::Decl),
    Expr(Option<exprs::Expr>),
}

impl From<decls::Decl> for ForInit {
    fn from(value: decls::Decl) -> Self {
        Self::Decl(value)
    }
}

impl From<exprs::Expr> for ForInit {
    fn from(value: exprs::Expr) -> Self {
        Self::Expr(Some(value))
    }
}

struct If {
    cond: exprs::Expr,
    body: Box<Self>,
    otherwise: Option<Box<Self>>,
}

struct Label {
    name: String,
    body: Box<Stmt>,
}

impl Label {
    #[rustfmt::skip]
    fn new(name: String, body: Stmt) -> Self {
        Self { name, body: Box::new(body) }
    }
}

struct While {
    id: String,
    cond: exprs::Expr,
    body: Box<Stmt>,
}

enum Case {
    Int(u64, String),
    Default(String),
}

struct Switch {
    id: String,
    value: exprs::Expr,
    body: Box<Stmt>,
    cases: Vec<Case>,
}

struct Clause {
    parent: String,
    value: exprs::Expr,
    body: Box<Stmt>,
}

struct Default {
    parent: String,
    body: Box<Stmt>,
}

struct For {
    id: String,
    init: ForInit,
    cond: Option<exprs::Expr>,
    post: Option<exprs::Expr>,
    body: Box<Stmt>,
}

pub enum Stmt {
    Null,
    Return(exprs::Expr),
    Expr(exprs::Expr),
    If(If),
    Goto(String),
    Label(Label),
    Comp(blocks::Block),
    Break(Option<String>),
    Continue(Option<String>),
    While(While),
    DoWhile(While),
    Switch(Switch),
    Case(Clause),
    Default(Default),
    For(For),
}

fn consume_null(mut state: ParserState) -> ParseResult<Stmt> {
    match state.tokens.pop_front() {
        Some(Token::Semicolon) => Ok((state, Stmt::Null)),
        token => Err(expected(Token::Semicolon, token)),
    }
}

fn consume_return(mut state: ParserState) -> ParseResult<Stmt> {
    match state.tokens.pop_front() {
        Some(Token::Return) => {
            let (mut state, expr) = exprs::parse(state)?;
            match state.tokens.pop_front() {
                Some(Token::Semicolon) => Ok((state, Stmt::Return(expr))),
                token => Err(expected(Token::Semicolon, token)),
            }
        }
        token => Err(expected(Token::Return, token)),
    }
}

fn consume_break(mut state: ParserState) -> ParseResult<Stmt> {
    match state.tokens.pop_front() {
        Some(Token::Break) => match state.tokens.pop_front() {
            Some(Token::Semicolon) => Ok((state, Stmt::Break(None))),
            token => Err(expected(Token::Semicolon, token)),
        },
        token => Err(expected(Token::Break, token)),
    }
}

fn consume_continue(mut state: ParserState) -> ParseResult<Stmt> {
    match state.tokens.pop_front() {
        Some(Token::Continue) => {
            let label = state.current_loop().map(|scope| scope.label().into());
            match state.tokens.pop_front() {
                Some(Token::Semicolon) => Ok((state, Stmt::Continue(label))),
                token => Err(expected(Token::Semicolon, token)),
            }
        }
        token => Err(expected(Token::Continue, token)),
    }
}

fn consume_label(mut state: ParserState) -> ParseResult<Stmt> {
    match state.tokens.pop_front() {
        Some(Token::Ident(ident)) => match state.tokens.pop_front() {
            Some(Token::Colon) => {
                let (state, stmt) = parse(state)?;
                Ok((state, Stmt::Label(Label::new(ident, stmt))))
            }
            token => Err(expected(Token::Colon, token)),
        },
        token => Err(expected(Token::Ident("identifier".into()), token)),
    }
}

fn consume_goto(mut state: ParserState) -> ParseResult<Stmt> {
    match state.tokens.pop_front() {
        Some(Token::Goto) => match state.tokens.pop_front() {
            Some(Token::Ident(ident)) => match state.tokens.pop_front() {
                Some(Token::Semicolon) => Ok((state, Stmt::Goto(ident))),
                token => Err(expected(Token::Semicolon, token)),
            },
            token => Err(expected(Token::Ident("identifier".into()), token)),
        },
        token => Err(expected(Token::Goto, token)),
    }
}

fn consume_block(mut state: ParserState) -> ParseResult<Stmt> {
    match state.tokens.pop_front() {
        Some(Token::LBrace) => {
            let (mut state, block) = blocks::parse(state)?;
            match state.tokens.pop_front() {
                Some(Token::RBrace) => Ok((state, Stmt::Comp(block))),
                token => Err(expected(Token::RBrace, token)),
            }
        }
        token => Err(expected(Token::LBrace, token)),
    }
}

fn consume_if(mut state: ParserState) -> ParseResult<Stmt> {
    match state.tokens.pop_front() {
        Some(Token::If) => match state.tokens.pop_front() {
            Some(Token::LParen) => {
                let (mut state, cond) = exprs::parse(state)?;
                match state.tokens.pop_front() {
                    Some(Token::RParen) => {
                        let (mut state, then) = parse(state)?;
                        todo!()
                    }
                    token => Err(expected(Token::RParen, token)),
                }
            }
            token => Err(expected(Token::RParen, token)),
        },
        token => Err(expected(Token::If, token)),
    }
}

pub fn parse(state: ParserState) -> ParseResult<Stmt> {
    let token = state.tokens.front();
    match token.ok_or("Unexpected end of input: expected stmt")? {
        Token::Semicolon => consume_null(state),
        Token::LBrace => consume_block(state),
        Token::Return => consume_return(state),
        Token::If => consume_if(state),
        Token::Break => consume_break(state),
        Token::Continue => consume_continue(state),
        Token::Goto => consume_goto(state),
        Token::Ident(_) => consume_label(state),
        _ => todo!(),
    }
}
