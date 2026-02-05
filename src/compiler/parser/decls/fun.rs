use super::super::super::tokenizer::Token;
use super::super::{ParseResult, ParserState, blocks};

pub struct Decl {
    name: String,
    params: Vec<String>,
    body: Option<blocks::Block>,
}

impl Decl {
    #[rustfmt::skip]
    fn new(name: String, params: Vec<String>, body: Option<blocks::Block>) -> Self {
        Self { name, params, body }
    }
}

fn consume_param(mut state: ParserState) -> ParseResult<String> {
    match state.tokens.pop_front() {
        Some(Token::Int) => match state.tokens.pop_front() {
            Some(Token::Ident(ident)) => Ok((state, ident)),
            Some(token) => Err(format!("Expected identifier found: {token}")),
            None => Err(String::from("Unexpected end of input: expected identifier")),
        },
        Some(token) => Err(format!("Expected `int` found: {token}")),
        None => Err(String::from("Unexpected end of input: expected `int`")),
    }
}

fn consume_params(mut state: ParserState) -> ParseResult<Vec<String>> {
    if let Some(Token::Void) = state.tokens.front() {
        let _ = state.tokens.pop_front();
        Ok((state, Vec::new()))
    } else {
        let (mut state, mut param) = consume_param(state)?;
        let mut params = vec![param];
        while state.tokens.front().is_some_and(|t| t.ne(&Token::RParen)) {
            match state.tokens.pop_front() {
                Some(Token::Comma) => {
                    (state, param) = consume_param(state)?;
                    params.push(param);
                }
                Some(token) => return Err(format!("Expected `,` found: {token}")),
                None => return Err(String::from("Unexpected end of input: expected: `,`")),
            }
        }
        if params.len() > 0 {
            Ok((state, params))
        } else {
            Err("At least one parameter or void must be provided".into())
        }
    }
}

pub fn parse(mut state: ParserState) -> ParseResult<Decl> {
    match state.tokens.pop_front() {
        Some(Token::Int) => match state.tokens.pop_front() {
            Some(Token::Ident(ident)) => match state.tokens.pop_front() {
                Some(Token::LParen) => {
                    let (mut state, params) = consume_params(state)?;
                    match state.tokens.pop_front() {
                        Some(Token::RParen) => {
                            if let Some(Token::Semicolon) = state.tokens.front() {
                                let _ = state.tokens.pop_front();
                                Ok((state, Decl::new(ident, params, None)))
                            } else {
                                let (state, body) = blocks::parse(state)?;
                                Ok((state, Decl::new(ident, params, Some(body))))
                            }
                        }
                        Some(token) => Err(format!("Expected `)` or intializer found: {token}")),
                        None => Err(String::from("Unexpected end of input: expected `)`")),
                    }
                }
                Some(token) => Err(format!("Expected `(` or intializer found: {token}")),
                None => Err(String::from("Unexpected end of input: expected `(`")),
            },
            Some(token) => Err(format!("Expected identifier found: {token}")),
            None => Err(String::from("Unexpected end of input: expected identifier")),
        },
        Some(token) => Err(format!("Expected `int` found: {token}")),
        None => Err(String::from("Unexpected end of input: expected `int`")),
    }
}
