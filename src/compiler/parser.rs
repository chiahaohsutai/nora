use std::collections::{HashSet, VecDeque};

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
    jumps: HashSet<String>,
    labels: VecDeque<String>,
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

pub struct Program(Vec<decls::fun::Decl>);

pub fn parse(tokens: Vec<Token>) -> Result<Program, String> {
    todo!()
}
