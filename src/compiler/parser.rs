use std::borrow::Borrow;
use std::collections::{HashMap, HashSet, VecDeque};
use std::hash::Hash;

use tracing::instrument;

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

#[derive(Debug, Clone, PartialEq, Eq)]
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

#[derive(Debug, Clone)]
struct VarMap<K, V>(Vec<HashMap<K, V>>);

impl<K, V> VarMap<K, V>
where
    K: Eq + Hash,
{
    fn new() -> Self {
        Self(Vec::new())
    }
    fn find<Q: ?Sized>(&self, k: &Q) -> Option<&V>
    where
        K: Borrow<Q>,
        Q: Hash + Eq,
    {
        for env in self.0.iter().rev() {
            if let Some(v) = env.get(k) {
                return Some(v);
            }
        }
        None
    }
    fn insert(&mut self, k: K, v: V) -> Option<V>
    where
        K: Eq + Hash,
    {
        self.0.last_mut().map(|map| map.insert(k, v)).flatten()
    }

    fn enter(&mut self) {
        self.0.push(HashMap::new());
    }

    fn exit(&mut self) {
        let _ = self.0.pop();
    }
}

#[derive(Debug, Clone)]
struct ParserState {
    tokens: VecDeque<Token>,
    scopes: VecDeque<Scope>,
    jumps: HashSet<String>,
    labels: HashSet<String>,
    dups: HashSet<String>,
    errors: Vec<String>,
    vars: VarMap<String, String>,
}

impl ParserState {
    fn new(tokens: VecDeque<Token>) -> Self {
        Self {
            tokens,
            scopes: VecDeque::new(),
            jumps: HashSet::new(),
            labels: HashSet::new(),
            dups: HashSet::new(),
            errors: Vec::new(),
            vars: VarMap::new(),
        }
    }

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

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Program(Vec<decls::fun::Decl>);

#[instrument]
pub fn parse(tokens: Vec<Token>, resolve: bool) -> Result<Program, String> {
    let tokens = VecDeque::from(tokens);
    let mut state = ParserState::new(tokens);
    let mut funs = Vec::new();

    while state.tokens.front().is_some() {
        let definition = decls::fun::parse(state)?;
        state = definition.0;
        funs.push(definition.1);
    }

    if resolve {
        if state.dups.len() > 0 {
            let dups = state.dups;
            return Err(format!("Found at least one duplicate label: {dups:?}"));
        }
        if !state.jumps.is_subset(&state.labels) {
            return Err("Found at least one jump stmt with no corresponding label".into());
        }
        if state.errors.len() > 0 {
            let error = format!(
                "Found the following errors while parsing:\n{}",
                state.errors.join("\n")
            );
            return Err(error);
        }
    }

    Ok(Program(funs))
}
