use std::collections::{HashSet, VecDeque};

use super::generate_tag;
use super::tokenizer::Token;

mod blocks;
mod decls;
mod exprs;
mod factors;
mod stmts;

type ParserResult<T> = Result<(T, State), String>;

trait TupleExt<T, U> {
    fn map_first<K, F>(self, f: F) -> (K, U)
    where
        F: FnOnce(T) -> K;
}

impl<T, U> TupleExt<T, U> for (T, U) {
    fn map_first<K, F>(self, f: F) -> (K, U)
    where
        F: FnOnce(T) -> K,
    {
        (f(self.0), self.1)
    }
}

enum Scope {
    Loop(String),
    Switch(String),
}

impl AsRef<str> for Scope {
    fn as_ref(&self) -> &str {
        match self {
            Self::Loop(label) => label,
            Self::Switch(label) => label,
        }
    }
}

impl From<&Scope> for String {
    fn from(value: &Scope) -> Self {
        String::from(value.as_ref())
    }
}

struct State {
    tokens: VecDeque<Token>,
    scopes: VecDeque<Scope>,
    labels: HashSet<String>,
}

impl State {
    pub fn current_loop(&self) -> Option<&Scope> {
        self.scopes
            .iter()
            .rev()
            .find(|scope| matches!(scope, Scope::Loop(_)))
    }
}

fn generate_label() -> String {
    generate_tag("label")
}

pub struct Program(Vec<decls::FnDecl>);

pub fn parse(tokens: Vec<Token>) -> Result<Program, String> {
    todo!()
}
