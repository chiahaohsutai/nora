use super::{ParseResult, ParserState, blocks, exprs};

pub mod fun;
pub mod var;

pub enum Decl {
    FnDecl(fun::Decl),
    VarDecl(var::Decl),
}

pub fn parse(state: ParserState) -> ParseResult<Decl> {
    todo!()
}
