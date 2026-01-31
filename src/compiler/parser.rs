use std::collections::{HashSet, VecDeque};

use super::generate_tag;
use super::tokenizer::Token;

mod blocks;
mod decls;
mod exprs;
mod factors;
mod stmts;

type ParseResult<T> = Result<(ParserState, T), String>;

trait TupleExt<T, U> {
    fn mapl<K, F>(self, f: F) -> (K, U)
    where
        F: FnOnce(T) -> K;

    fn mapr<K, F>(self, f: F) -> (T, K)
    where
        F: FnOnce(U) -> K;
}

impl<T, U> TupleExt<T, U> for (T, U) {
    fn mapl<K, F: FnOnce(T) -> K>(self, f: F) -> (K, U) {
        (f(self.0), self.1)
    }

    fn mapr<K, F: FnOnce(U) -> K>(self, f: F) -> (T, K) {
        (self.0, f(self.1))
    }
}

trait Pipe<T> {
    fn pipe<U, F>(self, f: F) -> ParseResult<U>
    where
        F: FnOnce(ParserState, T) -> ParseResult<U>;
}

impl<T> Pipe<T> for ParseResult<T> {
    fn pipe<U, F: FnOnce(ParserState, T) -> ParseResult<U>>(self, f: F) -> ParseResult<U> {
        match self {
            Ok((state, value)) => f(state, value),
            Err(e) => Err(e),
        }
    }
}

fn consume(mut state: ParserState) -> ParseResult<Token> {
    match state.tokens.pop_front() {
        Some(token) => Ok((state, token)),
        None => Err(String::from("Unexpected end of input")),
    }
}

fn expect<F>(state: ParserState, token: Token, pred: F) -> ParseResult<Token>
where
    F: FnOnce(&Token) -> bool,
{
    if pred(&token) {
        Ok((state, token))
    } else {
        Err(format!("Unexpected token: `{token}`"))
    }
}

fn consume_then<F, T>(state: ParserState, f: F) -> ParseResult<T>
where
    F: FnOnce(ParserState, Token) -> ParseResult<T>,
{
    consume(state).pipe(f)
}

fn consume_expect_then<F, P, T>(state: ParserState, pred: P, f: F) -> ParseResult<T>
where
    P: FnOnce(&Token) -> bool,
    F: FnOnce(ParserState, Token) -> ParseResult<T>,
{
    consume(state).pipe(|s, t| expect(s, t, pred)).pipe(f)
}

enum Scope {
    Loop(String),
    Switch(String),
}

impl Scope {
    pub fn label(&self) -> &str {
        match self {
            Self::Loop(label) => label,
            Self::Switch(label) => label,
        }
    }
}

struct ParserState {
    tokens: VecDeque<Token>,
    scopes: VecDeque<Scope>,
    labels: HashSet<String>,
}

impl ParserState {
    pub fn current_loop(&self) -> Option<&Scope> {
        self.scopes
            .iter()
            .rev()
            .find(|scope| matches!(scope, Scope::Loop(_)))
    }

    pub fn current_switch(&self) -> Option<&Scope> {
        self.scopes
            .iter()
            .rev()
            .find(|scope| matches!(scope, Scope::Switch(_)))
    }
}

fn generate_label() -> String {
    generate_tag("label")
}

pub struct Program(Vec<decls::FnDecl>);

pub fn parse(tokens: Vec<Token>) -> Result<Program, String> {
    todo!()
}
