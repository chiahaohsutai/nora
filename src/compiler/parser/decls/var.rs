use tracing::{debug, instrument};

use super::super::super::{generate_tag, tokenizer::Token};
use super::super::{ParseResult, ParserState, exprs};

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Decl {
    id: String,
    name: String,
    expr: Option<exprs::Expr>,
}

impl Decl {
    #[rustfmt::skip]
    fn new(id: String, name: String, expr: Option<exprs::Expr>) -> Self {
        Self { id, name, expr }
    }
}

#[instrument]
pub fn parse(mut state: ParserState) -> ParseResult<Decl> {
    debug!("Consuming variable declaration");
    match state.tokens.pop_front() {
        Some(Token::Int) => match state.tokens.pop_front() {
            Some(Token::Ident(ident)) => {
                let id = generate_tag(format!("var.{ident}"));
                match state.tokens.pop_front() {
                    Some(Token::Semicolon) => {
                        if let Some(_) = state.vars.insert(ident.clone(), id.clone()) {
                            let e = format!("Duplicate variable declaration: {ident}");
                            state.errors.push(e);
                        }
                        Ok((state, Decl::new(id, ident, None)))
                    }
                    Some(Token::Eq) => {
                        let (mut state, expr) = exprs::parse(state)?;
                        match state.tokens.pop_front() {
                            Some(Token::Semicolon) => {
                                if let Some(_) = state.vars.insert(ident.clone(), id.clone()) {
                                    let e = format!("Duplicate variable declaration: {ident}");
                                    state.errors.push(e);
                                }
                                Ok((state, Decl::new(id, ident, Some(expr))))
                            }
                            Some(token) => Err(format!("Expected `;` found: {token}")),
                            None => Err("Unexpected end of input: expected `;`".into()),
                        }
                    }
                    Some(token) => Err(format!("Expected `;` or intializer found: {token}")),
                    None => Err("Unexpected end of input: expected `;` or intializer".into()),
                }
            }
            Some(token) => Err(format!("Expected identifier found: {token}")),
            None => Err(String::from("Unexpected end of input: expected identifier")),
        },
        Some(token) => Err(format!("Expected `int` found: {token}")),
        None => Err(String::from("Unexpected end of input: expected `int`")),
    }
}
