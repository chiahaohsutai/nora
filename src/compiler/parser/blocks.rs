use crate::compiler::parser::State;

use super::{ParserResult, decls, stmts};

type BlockResult = ParserResult<Block>;

enum BlockItem {
    Stmt(stmts::Stmt),
    Decl(decls::Decl),
}

pub struct Block {
    items: Vec<BlockItem>,
}

pub fn parse(state: State) -> BlockResult {
    todo!()
}
