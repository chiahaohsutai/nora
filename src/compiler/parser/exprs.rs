use tracing::instrument;

use super::super::tokenizer::Token;
use super::{ParseResult, ParserState, TupleExt, factors};

#[derive(Debug, Clone, PartialEq, Eq, Copy)]
enum BinOp {
    Add,
    Div,
    Sub,
    Mul,
    Rem,
    BitOr,
    BitAnd,
    BitXor,
    LShift,
    RShift,
    And,
    Or,
    EqEq,
    NotEq,
    Lt,
    Gt,
    Le,
    Ge,
    Assign,
    AddAssign,
    SubAssign,
    DivAssign,
    MulAssign,
    RemAssign,
    AndAssign,
    OrAssign,
    XorAssign,
    LShiftAssign,
    RShiftAssign,
}

impl BinOp {
    fn precedence(&self) -> u64 {
        match self {
            Self::Assign
            | Self::SubAssign
            | Self::DivAssign
            | Self::AddAssign
            | Self::RemAssign
            | Self::MulAssign
            | Self::AndAssign
            | Self::OrAssign
            | Self::XorAssign
            | Self::LShiftAssign
            | Self::RShiftAssign => 1,
            Self::Or => 5,
            Self::And => 10,
            Self::BitOr => 15,
            Self::BitXor => 20,
            Self::BitAnd => 25,
            Self::EqEq | Self::NotEq => 30,
            Self::Lt | Self::Gt | Self::Le | Self::Ge => 35,
            Self::RShift | Self::LShift => 40,
            Self::Add | Self::Sub => 45,
            Self::Mul | Self::Div | Self::Rem => 50,
        }
    }
}

impl TryFrom<&Token> for BinOp {
    type Error = String;

    fn try_from(value: &Token) -> Result<Self, Self::Error> {
        match value {
            Token::Plus => Ok(Self::Add),
            Token::Minus => Ok(Self::Sub),
            Token::Star => Ok(Self::Mul),
            Token::Slash => Ok(Self::Div),
            Token::Percent => Ok(Self::Rem),
            Token::Amp => Ok(Self::BitAnd),
            Token::Pipe => Ok(Self::BitOr),
            Token::Caret => Ok(Self::BitXor),
            Token::Shl => Ok(Self::LShift),
            Token::Shr => Ok(Self::RShift),
            Token::AmpAmp => Ok(Self::And),
            Token::PipePipe => Ok(Self::Or),
            Token::EqEq => Ok(Self::EqEq),
            Token::NotEq => Ok(Self::NotEq),
            Token::Lt => Ok(Self::Lt),
            Token::Gt => Ok(Self::Gt),
            Token::Le => Ok(Self::Le),
            Token::Ge => Ok(Self::Ge),
            Token::Eq => Ok(Self::Assign),
            Token::PlusEq => Ok(Self::AddAssign),
            Token::MinusEq => Ok(Self::SubAssign),
            Token::SlashEq => Ok(Self::DivAssign),
            Token::PercentEq => Ok(Self::RemAssign),
            Token::StarEq => Ok(Self::MulAssign),
            Token::AmpEq => Ok(Self::AndAssign),
            Token::PipeEq => Ok(Self::OrAssign),
            Token::CaretEq => Ok(Self::XorAssign),
            Token::ShlEq => Ok(Self::LShiftAssign),
            Token::ShrEq => Ok(Self::RShiftAssign),
            token => Err(format!("Expected binary operator found: {token}")),
        }
    }
}

impl TryFrom<Option<&Token>> for BinOp {
    type Error = String;

    fn try_from(value: Option<&Token>) -> Result<Self, Self::Error> {
        if let Some(token) = value {
            Self::try_from(token)
        } else {
            Err(format!("Expected binary operator"))
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Ternary {
    cond: Box<Expr>,
    then: Box<Expr>,
    otherwise: Box<Expr>,
}

impl Ternary {
    fn new(cond: Expr, then: Expr, otherwise: Expr) -> Self {
        Self {
            cond: Box::new(cond),
            then: Box::new(then),
            otherwise: Box::new(otherwise),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct BinExpr {
    lhs: Box<Expr>,
    op: BinOp,
    rhs: Box<Expr>,
}

impl BinExpr {
    #[rustfmt::skip]
    fn new(lhs: Expr, op: BinOp, rhs: Expr) -> Self {
        Self { lhs: Box::new(lhs), op, rhs: Box::new(rhs) }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Expr {
    Fac(factors::Factor),
    Tern(Ternary),
    Bin(BinExpr),
}

impl From<factors::Factor> for Expr {
    fn from(value: factors::Factor) -> Self {
        Self::Fac(value)
    }
}

fn is_assignment(op: &BinOp) -> bool {
    matches!(
        op,
        BinOp::Assign
            | BinOp::AddAssign
            | BinOp::SubAssign
            | BinOp::DivAssign
            | BinOp::RemAssign
            | BinOp::MulAssign
            | BinOp::AndAssign
            | BinOp::OrAssign
            | BinOp::XorAssign
            | BinOp::RShiftAssign
            | BinOp::LShiftAssign
    )
}

#[derive(Debug, Clone, PartialEq, Eq, Copy)]
enum Op {
    Ternary,
    BinOp(BinOp),
}

impl Op {
    fn precedence(&self) -> u64 {
        match self {
            Self::BinOp(op) => op.precedence(),
            _ => 0,
        }
    }
}

impl TryFrom<&Token> for Op {
    type Error = String;

    fn try_from(value: &Token) -> Result<Self, Self::Error> {
        match value {
            Token::Eroteme => Ok(Self::Ternary),
            token => BinOp::try_from(token).map(|op| Self::BinOp(op)),
        }
    }
}

fn consume_ternary(mut state: ParserState, operand: Expr) -> ParseResult<Expr> {
    let token = state.tokens.pop_front();
    match token.ok_or("Unexpected end of input: expected expression")? {
        Token::Eroteme => {
            let (mut state, then) = consume_and_climb(state, 0)?;
            let (state, otherwise) = match state.tokens.pop_front() {
                Some(Token::Colon) => consume_and_climb(state, 0)?,
                Some(token) => return Err(format!("Expected `:` found: {token}")),
                None => return Err(format!("Unexpected end of input: expected `:`")),
            };
            Ok((state, Expr::Tern(Ternary::new(operand, then, otherwise))))
        }
        token => Err(format!("Expected `?` found: {token}")),
    }
}

fn consume_expr(mut state: ParserState, operand: Expr) -> ParseResult<Expr> {
    let token = state
        .tokens
        .pop_front()
        .ok_or(String::from("Unexpected end of input: expected expression"))?;

    let op = BinOp::try_from(&token)?;
    let precedence = if is_assignment(&op) {
        op.precedence()
    } else {
        op.precedence() + 1
    };

    let (state, expr) = consume_and_climb(state, precedence)?;
    Ok((state, Expr::Bin(BinExpr::new(operand, op, expr))))
}

fn consume_and_climb(state: ParserState, precedence: u64) -> ParseResult<Expr> {
    let (mut state, mut lhs) = factors::parse(state)?.mapr(Expr::from);
    let mut op = state.tokens.front().map(|t| Op::try_from(t).ok()).flatten();

    while op.as_ref().is_some_and(|op| op.precedence() >= precedence) {
        (state, lhs) = match op.as_ref().unwrap() {
            Op::Ternary => consume_ternary(state, lhs)?,
            Op::BinOp(_) => consume_expr(state, lhs)?,
        };
        op = state.tokens.front().map(|t| Op::try_from(t).ok()).flatten()
    }
    Ok((state, lhs.into()))
}

#[instrument]
pub fn parse(state: ParserState) -> ParseResult<Expr> {
    consume_and_climb(state, 0)
}
