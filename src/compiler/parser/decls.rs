use tracing::{debug, instrument};

use super::super::tokenizer::Token;
use super::{ParseResult, ParserState};

pub mod fun;
pub mod var;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Decl {
    FnDecl(fun::Decl),
    VarDecl(var::Decl),
}

#[instrument]
pub fn parse(state: ParserState) -> ParseResult<Decl> {
    debug!("Consuming declaration");
    if let Some(Token::LParen) = state.tokens.get(2) {
        let (state, decl) = fun::parse(state)?;
        Ok((state, Decl::FnDecl(decl)))
    } else {
        let (state, decl) = var::parse(state)?;
        Ok((state, Decl::VarDecl(decl)))
    }
}
