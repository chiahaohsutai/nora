use std::fmt;

use super::super::tokenizer::Token;
use super::{ParseResult, ParserState, TupleExt, exprs};

enum UnaryOp {
    Decr,
    Incr,
    Neg,
    Not,
    Compl,
}

impl TryFrom<&Token> for UnaryOp {
    type Error = String;

    fn try_from(value: &Token) -> Result<Self, Self::Error> {
        match value {
            Token::MinusMinus => Ok(Self::Decr),
            Token::PlusPlus => Ok(Self::Incr),
            Token::Minus => Ok(Self::Neg),
            Token::Bang => Ok(Self::Not),
            Token::Tilde => Ok(Self::Compl),
            token => return Err(format!("Expected unary operator found: {token}")),
        }
    }
}

impl fmt::Display for UnaryOp {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Decr => write!(f, "--"),
            Self::Incr => write!(f, "++"),
            Self::Neg => write!(f, "-"),
            Self::Not => write!(f, "!"),
            Self::Compl => write!(f, "~"),
        }
    }
}

enum Fixity {
    Prefix,
    Postfix,
}

struct Unary {
    op: UnaryOp,
    fixity: Fixity,
    operand: Box<Factor>,
}

impl Unary {
    #[rustfmt::skip]
    fn new(op: UnaryOp, fixity: Fixity, operand: Factor) -> Self {
        let operand = Box::new(operand);
        Self { op, fixity, operand }
    }
}

struct FnCall {
    name: String,
    args: Vec<exprs::Expr>,
}

pub enum Factor {
    Int(u64),
    Ident(String),
    Expr(Box<exprs::Expr>),
    FnCall(FnCall),
    Unary(Unary),
}

impl From<exprs::Expr> for Factor {
    fn from(value: exprs::Expr) -> Self {
        Self::Expr(Box::new(value))
    }
}

impl From<u64> for Factor {
    fn from(value: u64) -> Self {
        Self::Int(value)
    }
}

impl From<String> for Factor {
    fn from(value: String) -> Self {
        Self::Ident(value)
    }
}

impl From<FnCall> for Factor {
    fn from(value: FnCall) -> Self {
        Self::FnCall(value)
    }
}

impl From<Unary> for Factor {
    fn from(value: Unary) -> Self {
        Self::Unary(value)
    }
}

fn consume_args(mut state: ParserState) -> ParseResult<Factor> {
    todo!()
}

fn consume_ident(mut state: ParserState) -> ParseResult<Factor> {
    match state.tokens.pop_front() {
        Some(Token::Ident(ident)) => match state.tokens.front() {
            Some(Token::MinusMinus | Token::PlusPlus) => {
                let op = UnaryOp::try_from(&state.tokens.pop_front().unwrap())?;
                let factor = Factor::Ident(ident);
                Ok((state, Unary::new(op, Fixity::Postfix, factor).into()))
            }
            Some(Token::LParen) => consume_args(state),
            _ => Ok((state, Factor::Ident(ident))),
        },
        Some(token) => Err(format!("Expected identifier found: {token}")),
        None => Err(String::from("Unexpected end of input: expected factor")),
    }
}

fn consume_unary(mut state: ParserState) -> ParseResult<Factor> {
    let token = state
        .tokens
        .pop_front()
        .ok_or(String::from("Unexpected end of input: expected factor"))?;
    let op = UnaryOp::try_from(&token)?;
    Ok(parse(state)?.mapr(|f| Unary::new(op, Fixity::Prefix, f).into()))
}

fn consume_const(mut state: ParserState) -> ParseResult<Factor> {
    match state.tokens.pop_front() {
        Some(Token::Const(constant)) => Ok((state, Factor::Int(constant))),
        Some(token) => Err(format!("Expected constant found: {token}")),
        None => Err(String::from("Unexpected end of input: expected factor")),
    }
}

fn consume_expr(mut state: ParserState) -> ParseResult<Factor> {
    match state.tokens.pop_front() {
        Some(Token::LParen) => Ok(exprs::parse(state)?.mapr(|e| e.into())),
        Some(token) => Err(format!("Expected `(` found: {token}")),
        None => Err(String::from("Unexpected end of input: expected factor")),
    }
}

pub fn parse(state: ParserState) -> ParseResult<Factor> {
    let token = state.tokens.front();
    match token.ok_or("Unexpected end of input: expected factor")? {
        Token::Const(_) => consume_const(state),
        Token::Ident(_) => consume_ident(state),
        Token::LParen => consume_expr(state),
        Token::MinusMinus => consume_unary(state),
        Token::PlusPlus => consume_unary(state),
        Token::Minus => consume_unary(state),
        Token::Bang => consume_unary(state),
        Token::Tilde => consume_unary(state),
        token => return Err(format!("Invalid factor: unexpected token `{token}`")),
    }
}
