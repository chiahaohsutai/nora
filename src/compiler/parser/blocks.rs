use tracing::{debug, instrument};

use super::super::tokenizer::Token;
use super::{ParseResult, ParserState, decls, stmts};

#[derive(Debug, Clone, PartialEq, Eq)]
enum BlockItem {
    Stmt(stmts::Stmt),
    Decl(decls::Decl),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Block {
    items: Vec<BlockItem>,
}

impl Block {
    fn new(items: Vec<BlockItem>) -> Self {
        Self { items }
    }
}

fn consume_item(state: ParserState) -> ParseResult<BlockItem> {
    debug!("Consuming block item");
    match state.tokens.front() {
        Some(Token::Int) => {
            let (state, decl) = decls::parse(state)?;
            Ok((state, BlockItem::Decl(decl)))
        }
        Some(_) => {
            let (state, stmt) = stmts::parse(state)?;
            Ok((state, BlockItem::Stmt(stmt)))
        }
        None => Err(String::from("Unexpected end of input: expected block item")),
    }
}

#[instrument]
pub fn parse(mut state: ParserState) -> ParseResult<Block> {
    debug!("Consuming block");
    match state.tokens.pop_front() {
        Some(Token::LBrace) => {
            let mut items = vec![];
            while state.tokens.front().is_some_and(|t| t.ne(&Token::RBrace)) {
                let item = consume_item(state)?;
                state = item.0;
                items.push(item.1);
            }
            if let Some(Token::RBrace) = state.tokens.pop_front() {
                Ok((state, Block::new(items)))
            } else {
                Err(String::from("Unexpected end of input: expected `}`"))
            }
        }
        Some(token) => Err(format!("Expected `{{` found: {token}")),
        None => Err(String::from("Unexpected end of input: expected `{`")),
    }
}
