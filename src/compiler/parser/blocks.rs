use super::{ParseResult, ParserState, decls, stmts};

enum BlockItem {
    Stmt(stmts::Stmt),
    Decl(decls::Decl),
}

pub struct Block {
    items: Vec<BlockItem>,
}

pub fn parse(state: ParserState) -> ParseResult<Block> {
    todo!()
}
