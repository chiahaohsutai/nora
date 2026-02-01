use super::{ParseResult, ParserState, blocks, exprs};

struct VarDecl {
    name: String,
    expr: Option<exprs::Expr>,
}

pub struct FnDecl {
    name: String,
    params: Vec<String>,
    body: blocks::Block,
}

pub enum Decl {
    FnDecl(FnDecl),
    VarDecl(VarDecl),
}

pub fn parse(state: ParserState) -> ParseResult<Decl> {
    todo!()
}
