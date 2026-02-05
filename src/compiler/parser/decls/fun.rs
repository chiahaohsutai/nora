use super::super::{ParseResult, ParserState, blocks};

pub struct Decl {
    name: String,
    params: Vec<String>,
    body: blocks::Block,
}

impl Decl {
    #[rustfmt::skip]
    fn new(name: String, params: Vec<String>, body: blocks::Block) -> Self {
        Self { name, params, body }
    }
}

pub fn parse(state: ParserState) -> ParseResult<Decl> {
    todo!()
}
