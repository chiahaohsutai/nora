use super::super::tokenizer::Token;
use super::{ParseResult, ParserState, decls, stmts};

enum BlockItem {
    Stmt(stmts::Stmt),
    Decl(decls::Decl),
}

pub struct Block {
    items: Vec<BlockItem>,
}

impl Block {
    fn new(items: Vec<BlockItem>) -> Self {
        Self { items }
    }
}

// match tokens.pop_front() {
//             Some(tokenizer::Token::Delimiter(tokenizer::Delimiter::LeftBrace)) => {
//                 let mut block: Vec<BlockItem> = Vec::new();
//                 loop {
//                     if let Some(tokenizer::Token::Delimiter(tokenizer::Delimiter::RightBrace)) =
//                         tokens.front()
//                     {
//                         tokens.pop_front();
//                         break Ok(Self(block));
//                     }
//                     let item = BlockItem::parse(tokens)?;
//                     block.push(item);
//                 }
//             }
//             _ => Err(String::from("Expected '{' at the start of fn body.")),
//         }

fn consume_item(state: ParserState) -> ParseResult<BlockItem> {
    todo!()
}

pub fn parse(mut state: ParserState) -> ParseResult<Block> {
    match state.tokens.pop_front() {
        Some(Token::LBrace) => {
            let mut items = vec![];
            while state.tokens.front().is_some_and(|t| t.ne(&Token::RBrace)) {
                let item = consume_item(state)?;
                state = item.0;
                items.push(item.1);
            }
            Ok((state, Block::new(items)))
        }
        Some(token) => Err(format!("Expected `{{` found: {token}")),
        None => Err(String::from("Unexpected end of input: expected `{`")),
    }
}
