use std::fmt;

use tracing::{debug, instrument};

use super::super::tokenizer::Token;
use super::{ParseResult, ParserState, TupleExt, exprs};

#[derive(Debug, Clone, PartialEq, Eq, Copy)]
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

#[derive(Debug, Clone, PartialEq, Eq, Copy)]
enum Fixity {
    Prefix,
    Postfix,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Unary {
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

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct FnCall {
    name: String,
    args: Vec<exprs::Expr>,
}

impl FnCall {
    #[rustfmt::skip]
    fn new(name: String, args:  Vec<exprs::Expr>) -> Self {
        Self { name, args }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
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

fn consume_args(state: ParserState) -> ParseResult<Vec<exprs::Expr>> {
    debug!("Consuming args factor");
    let (mut state, mut expr) = exprs::parse(state)?;
    let mut args = vec![expr];
    while state.tokens.front().is_some_and(|t| t.ne(&Token::RParen)) {
        match state.tokens.pop_front() {
            Some(Token::Comma) => {
                (state, expr) = exprs::parse(state)?;
                args.push(expr);
            }
            Some(token) => return Err(format!("Expected `,` found: {token}")),
            None => return Err(String::from("Unexpected end of input: expected `,`")),
        }
    }
    Ok((state, args))
}

fn consume_ident(mut state: ParserState) -> ParseResult<Factor> {
    debug!("Consuming identity factor");
    match state.tokens.pop_front() {
        Some(Token::Ident(ident)) => match state.tokens.front() {
            Some(Token::LParen) => {
                let (mut state, args) = consume_args(state)?;
                match state.tokens.pop_front() {
                    Some(Token::RParen) => Ok((state, Factor::FnCall(FnCall::new(ident, args)))),
                    Some(token) => Err(format!("Expected `)` found: {token}")),
                    None => Err(String::from("Unexpected end of input: expected `)`")),
                }
            }
            _ => Ok((state, Factor::Ident(ident))),
        },
        Some(token) => Err(format!("Expected identifier found: {token}")),
        None => Err(String::from("Unexpected end of input: expected factor")),
    }
}

fn consume_unary(mut state: ParserState) -> ParseResult<Factor> {
    debug!("Consuming unary factor");
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
    debug!("Consuming expression factor");
    match state.tokens.pop_front() {
        Some(Token::LParen) => {
            let (mut state, expr) = exprs::parse(state)?.mapr(|e| e.into());
            match state.tokens.pop_front() {
                Some(Token::RParen) => Ok((state, expr)),
                Some(token) => Err(format!("Expected `)` found: {token}")),
                None => Err(String::from("Unexpected end of input: expected `)`")),
            }
        }
        Some(token) => Err(format!("Expected `(` found: {token}")),
        None => Err(String::from("Unexpected end of input: expected factor")),
    }
}

fn is_postfix_op(token: &Token) -> bool {
    matches!(token, Token::MinusMinus | Token::PlusPlus)
}

#[instrument]
pub fn parse(state: ParserState) -> ParseResult<Factor> {
    debug!("Consuming factor");
    let (mut state, mut factor) = match state.tokens.front() {
        Some(Token::Const(_)) => consume_const(state),
        Some(Token::Ident(_)) => consume_ident(state),
        Some(Token::LParen) => consume_expr(state),
        Some(Token::MinusMinus) => consume_unary(state),
        Some(Token::PlusPlus) => consume_unary(state),
        Some(Token::Minus) => consume_unary(state),
        Some(Token::Bang) => consume_unary(state),
        Some(Token::Tilde) => consume_unary(state),
        Some(token) => return Err(format!("Invalid factor: unexpected token `{token}`")),
        None => Err("Unexpected end of input: expected factor".into()),
    }?;
    while state.tokens.front().is_some_and(is_postfix_op) {
        let op = UnaryOp::try_from(&state.tokens.pop_front().unwrap())?;
        factor = Factor::Unary(Unary::new(op, Fixity::Postfix, factor))
    }
    Ok((state, factor))
}
