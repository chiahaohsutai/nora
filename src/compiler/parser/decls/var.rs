use super::super::super::tokenizer::Token;
use super::super::{ParseResult, ParserState, exprs};

pub struct Decl {
    name: String,
    expr: Option<exprs::Expr>,
}

impl Decl {
    #[rustfmt::skip]
    fn new(name: String, expr: Option<exprs::Expr>) -> Self {
        Self { name, expr }
    }
}

pub fn parse(mut state: ParserState) -> ParseResult<Decl> {
    match state.tokens.pop_front() {
        Some(Token::Int) => match state.tokens.pop_front() {
            Some(Token::Ident(ident)) => match state.tokens.pop_front() {
                Some(Token::Semicolon) => Ok((state, Decl::new(ident, None))),
                Some(Token::Eq) => {
                    let (state, expr) = exprs::parse(state)?;
                    Ok((state, Decl::new(ident, Some(expr))))
                }
                Some(token) => Err(format!("Expected `;` or intializer found: {token}")),
                None => Err("Unexpected end of input: expected `;` or intializer".into()),
            },
            Some(token) => Err(format!("Expected identifier found: {token}")),
            None => Err(String::from("Unexpected end of input: expected identifier")),
        },
        Some(token) => Err(format!("Expected `int` found: {token}")),
        None => Err(String::from("Unexpected end of input: expected `int`")),
    }
}
