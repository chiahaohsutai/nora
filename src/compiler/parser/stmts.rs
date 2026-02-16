use tracing::{debug, instrument};

use super::super::{generate_tag, tokenizer::Token};
use super::{ParseResult, ParserState, Scope, TupleExt, blocks, decls, exprs};

#[derive(Debug, Clone, PartialEq, Eq)]
enum ForInit {
    Decl(decls::var::Decl),
    Expr(Option<exprs::Expr>),
}

impl From<decls::var::Decl> for ForInit {
    fn from(value: decls::var::Decl) -> Self {
        Self::Decl(value)
    }
}

impl From<exprs::Expr> for ForInit {
    fn from(value: exprs::Expr) -> Self {
        Self::Expr(Some(value))
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct If {
    cond: exprs::Expr,
    body: Box<Stmt>,
    otherwise: Option<Box<Stmt>>,
}

impl If {
    #[rustfmt::skip]
    fn new(cond: exprs::Expr, body: Stmt, otherwise: Option<Stmt>) -> Self {
        let otherwise = otherwise.map(|stmt| Box::new(stmt));
        Self { cond, body: Box::new(body), otherwise }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Label {
    name: String,
    body: Box<Stmt>,
}

impl Label {
    #[rustfmt::skip]
    fn new(name: String, body: Stmt) -> Self {
        Self { name, body: Box::new(body) }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct While {
    id: String,
    cond: exprs::Expr,
    body: Box<Stmt>,
}

impl While {
    #[rustfmt::skip]
    fn new(id: &str, cond: exprs::Expr, body: Stmt) -> Self {
        Self { id: id.into(), cond, body: Box::new(body) }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
enum Case {
    Int(u64, String),
    Default(String),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Switch {
    id: String,
    value: exprs::Expr,
    body: Box<Stmt>,
    cases: Vec<Case>,
}

impl Switch {
    #[rustfmt::skip]
    fn new(id: String, value: exprs::Expr, body: Stmt) -> Self {
        Self { id, value, body: Box::new(body), cases: vec![]}
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Clause {
    parent: Option<String>,
    value: exprs::Expr,
    body: Box<Stmt>,
}

impl Clause {
    #[rustfmt::skip]
    fn new(parent: Option<String>, value: exprs::Expr, body: Stmt) -> Self {
        Self { parent, value, body: Box::new(body)}
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Default {
    parent: Option<String>,
    body: Box<Stmt>,
}

impl Default {
    #[rustfmt::skip]
    fn new(parent: Option<String>, body: Stmt) -> Self {
        Self { parent, body: Box::new(body)}
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct For {
    id: String,
    init: ForInit,
    cond: Option<exprs::Expr>,
    post: Option<exprs::Expr>,
    body: Box<Stmt>,
}

impl For {
    fn new(
        id: &str,
        init: ForInit,
        cond: Option<exprs::Expr>,
        post: Option<exprs::Expr>,
        body: Stmt,
    ) -> Self {
        Self {
            id: id.into(),
            init,
            cond,
            post,
            body: Box::new(body),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Stmt {
    Null,
    Return(exprs::Expr),
    Expr(exprs::Expr),
    If(If),
    Goto(String),
    Label(Label),
    Comp(blocks::Block),
    Break(Option<String>),
    Continue(Option<String>),
    While(While),
    DoWhile(While),
    Switch(Switch),
    Case(Clause),
    Default(Default),
    For(For),
}

#[instrument]
fn consume_null(mut state: ParserState) -> ParseResult<Stmt> {
    debug!("Consumeing null statment");
    match state.tokens.pop_front() {
        Some(Token::Semicolon) => Ok((state, Stmt::Null)),
        Some(token) => Err(format!("Expected `;` found: {token}")),
        None => Err(String::from("Unexpected end of input: expected `;`")),
    }
}

#[instrument]
fn consume_return(mut state: ParserState) -> ParseResult<Stmt> {
    debug!("Consuming return statment");
    match state.tokens.pop_front() {
        Some(Token::Return) => {
            let (mut state, expr) = exprs::parse(state)?;
            match state.tokens.pop_front() {
                Some(Token::Semicolon) => Ok((state, Stmt::Return(expr))),
                Some(token) => Err(format!("Expected `;` found: {token}")),
                None => Err(String::from("Unexpected end of input: expected `;`")),
            }
        }
        Some(token) => Err(format!("Expected `return` found: {token}")),
        None => Err(String::from("Unexpected end of input: expected `return`")),
    }
}

#[instrument]
fn consume_break(mut state: ParserState) -> ParseResult<Stmt> {
    debug!("Consuming break statment");
    match state.tokens.pop_front() {
        Some(Token::Break) => {
            let label = state.scopes.back().map(|scope| scope.label().into());
            if let None = label {
                let e = "Found break stmt with no parent scope";
                state.errors.push(e.into());
            }
            match state.tokens.pop_front() {
                Some(Token::Semicolon) => Ok((state, Stmt::Break(label))),
                Some(token) => Err(format!("Expected `;` found: {token}")),
                None => Err(String::from("Unexpected end of input: expected `;`")),
            }
        }
        Some(token) => Err(format!("Expected `break` found: {token}")),
        None => Err(String::from("Unexpected end of input: expected `{`")),
    }
}

#[instrument]
fn consume_continue(mut state: ParserState) -> ParseResult<Stmt> {
    debug!("Consuming continue statment");
    match state.tokens.pop_front() {
        Some(Token::Continue) => {
            let label = state.current_loop().map(|scope| scope.label().into());
            if let None = label {
                let e = "Found continue stmt outside loop scope";
                state.errors.push(e.into());
            }
            match state.tokens.pop_front() {
                Some(Token::Semicolon) => Ok((state, Stmt::Continue(label))),
                Some(token) => Err(format!("Expected `;` found: {token}")),
                None => Err(String::from("Unexpected end of input: expected `;`")),
            }
        }
        Some(token) => Err(format!("Expected `continue` found: {token}")),
        None => Err(String::from("Unexpected end of input: expected `continue`")),
    }
}

#[instrument]
fn consume_label(mut state: ParserState) -> ParseResult<Stmt> {
    debug!("Consuming label statment");
    match state.tokens.pop_front() {
        Some(Token::Ident(ident)) => match state.tokens.pop_front() {
            Some(Token::Colon) => {
                let (mut state, stmt) = parse(state)?;
                if !state.labels.insert(ident.clone()) {
                    state.dups.insert(ident.clone());
                };
                Ok((state, Stmt::Label(Label::new(ident, stmt))))
            }
            Some(token) => Err(format!("Expected `:` after label, found: {token}")),
            None => Err("Unexpected end of input after label: expected `:`".into()),
        },
        Some(token) => Err(format!("Expected identifier found: {token}")),
        None => Err(String::from("Unexpected end of input: expected identifier")),
    }
}

#[instrument]
fn consume_goto(mut state: ParserState) -> ParseResult<Stmt> {
    debug!("Consuming goto statment");
    match state.tokens.pop_front() {
        Some(Token::Goto) => match state.tokens.pop_front() {
            Some(Token::Ident(ident)) => match state.tokens.pop_front() {
                Some(Token::Semicolon) => {
                    let _ = state.jumps.insert(ident.clone());
                    Ok((state, Stmt::Goto(ident)))
                }
                Some(token) => Err(format!("Expected `;` found: {token}")),
                None => Err(String::from("Unexpected end of input: expected `;`")),
            },
            Some(token) => Err(format!("Expected identifier found: {token}")),
            None => Err(String::from("Unexpected end of input: expected identifier")),
        },
        Some(token) => Err(format!("Expected `goto` found: {token}")),
        None => Err(String::from("Unexpected end of input: expected `goto`")),
    }
}

#[instrument]
fn consume_block(state: ParserState) -> ParseResult<Stmt> {
    debug!("Consuming block statement");
    match state.tokens.front() {
        Some(Token::LBrace) => {
            let (state, block) = blocks::parse(state)?;
            Ok((state, Stmt::Comp(block)))
        }
        Some(token) => Err(format!("Expected `{{` found: {token}")),
        None => Err(String::from("Unexpected end of input: expected `{`")),
    }
}

#[instrument]
fn consume_cond(mut state: ParserState) -> ParseResult<exprs::Expr> {
    debug!("Consuming conditional statment");
    match state.tokens.pop_front() {
        Some(Token::LParen) => {
            let (mut state, cond) = exprs::parse(state)?;
            match state.tokens.pop_front() {
                Some(Token::RParen) => Ok((state, cond)),
                Some(token) => Err(format!("Expected `)` found: {token}")),
                None => Err(String::from("Unexpected end of input: expected `)`")),
            }
        }
        Some(token) => Err(format!("Expected `(` found: {token}")),
        None => Err(String::from("Unexpected end of input: expected `(`")),
    }
}

#[instrument]
fn consume_if(mut state: ParserState) -> ParseResult<Stmt> {
    debug!("Consuming if statment");
    match state.tokens.pop_front() {
        Some(Token::If) => {
            let (state, cond) = consume_cond(state)?;
            let (mut state, then) = parse(state)?;
            if let Some(Token::Else) = state.tokens.front() {
                let _ = state.tokens.pop_front();
                let (state, otherwise) = parse(state)?;
                Ok((state, Stmt::If(If::new(cond, then, Some(otherwise)))))
            } else {
                Ok((state, Stmt::If(If::new(cond, then, None))))
            }
        }
        Some(token) => Err(format!("Expected `if` found: {token}")),
        None => Err(String::from("Unexpected end of input: expected `if`")),
    }
}

#[instrument]
fn consume_while(mut state: ParserState) -> ParseResult<Stmt> {
    debug!("Consuming while statment");
    match state.tokens.pop_front() {
        Some(Token::While) => {
            let (state, cond) = consume_cond(state)?;
            let (mut state, body) = parse(state)?;
            let id = generate_tag("while.loop");
            state.scopes.push_back(Scope::Loop(id.to_string()));
            let stmt = Stmt::While(While::new(&id, cond, body));
            state.scopes.pop_back();
            Ok((state, stmt))
        }
        Some(token) => Err(format!("Expected `while` found: {token}")),
        None => Err(String::from("Unexpected end of input: expected `while`")),
    }
}

#[instrument]
fn consume_dowhile(mut state: ParserState) -> ParseResult<Stmt> {
    debug!("Consuming dowhile statment");
    match state.tokens.pop_front() {
        Some(Token::Do) => {
            let (mut state, body) = parse(state)?;
            match state.tokens.pop_front() {
                Some(Token::While) => {
                    let (mut state, cond) = consume_cond(state)?;
                    let id = generate_tag("do.while.loop");
                    state.scopes.push_back(Scope::Loop(id.to_string()));
                    match state.tokens.pop_front() {
                        Some(Token::Semicolon) => {
                            let stmt = Stmt::DoWhile(While::new(&id, cond, body));
                            state.scopes.pop_back();
                            Ok((state, stmt))
                        }
                        Some(token) => Err(format!("Expected `;` found: {token}")),
                        None => Err(String::from("Unexpected end of input: expected `;`")),
                    }
                }
                Some(token) => Err(format!("Expected `while` found: {token}")),
                None => Err(String::from("Unexpected end of input: expected `while`")),
            }
        }
        Some(token) => Err(format!("Expected `do` found: {token}")),
        None => Err(String::from("Unexpected end of input: expected `do`")),
    }
}

#[instrument]
fn consume_for_init(mut state: ParserState) -> ParseResult<ForInit> {
    debug!("Consuming forinit statment");
    match state.tokens.front() {
        Some(Token::Int) => {
            let (state, decl) = decls::var::parse(state)?.mapr(|decl| decl.into());
            Ok((state, decl))
        }
        Some(Token::Semicolon) => {
            let _ = state.tokens.pop_front();
            Ok((state, ForInit::Expr(None)))
        }
        Some(_) => {
            let (mut state, expr) = exprs::parse(state)?.mapr(|expr| expr.into());
            match state.tokens.pop_front() {
                Some(Token::Semicolon) => Ok((state, expr)),
                Some(token) => Err(format!("Expected `;` found: {token}")),
                None => Err(String::from("Unexpected end of input: expected `;`")),
            }
        }
        None => Err("Unexpected end of input: expected decl or expr".into()),
    }
}

#[instrument]
fn consume_for_cond(mut state: ParserState) -> ParseResult<Option<exprs::Expr>> {
    debug!("Consuming forcond statment");
    match state.tokens.front() {
        Some(Token::Semicolon) => {
            let _ = state.tokens.pop_front();
            Ok((state, None))
        }
        Some(_) => {
            let (mut state, expr) = exprs::parse(state)?;
            match state.tokens.pop_front() {
                Some(Token::Semicolon) => Ok((state, Some(expr))),
                Some(token) => Err(format!("Expected `;` found: {token}")),
                None => Err(String::from("Unexpected end of input: expected `;`")),
            }
        }
        None => Err("Unexpected end of input: expected expr or `;`".into()),
    }
}

#[instrument]
fn consume_for_post(state: ParserState) -> ParseResult<Option<exprs::Expr>> {
    debug!("Consuming forpost statment");
    match state.tokens.front() {
        Some(Token::RParen) => Ok((state, None)),
        Some(_) => {
            let (state, expr) = exprs::parse(state)?;
            Ok((state, Some(expr)))
        }
        None => Err("Unexpected end of input: expected expr or `;`".into()),
    }
}

#[instrument]
fn consume_for(mut state: ParserState) -> ParseResult<Stmt> {
    debug!("Consuming for statment");
    match state.tokens.pop_front() {
        Some(Token::For) => match state.tokens.pop_front() {
            Some(Token::LParen) => {
                let (state, init) = consume_for_init(state)?;
                let (state, cond) = consume_for_cond(state)?;
                let (mut state, post) = consume_for_post(state)?;
                match state.tokens.pop_front() {
                    Some(Token::RParen) => {
                        let (mut state, body) = parse(state)?;
                        let id = generate_tag("for.loop");
                        state.scopes.push_back(Scope::Loop(id.to_string()));
                        let stmt = Stmt::For(For::new(&id, init, cond, post, body));
                        state.scopes.pop_back();
                        Ok((state, stmt))
                    }
                    Some(token) => Err(format!("Expected `)` found: {token}")),
                    None => Err(String::from("Unexpected end of input: expected `)`")),
                }
            }
            Some(token) => Err(format!("Expected `(` found: {token}")),
            None => Err(String::from("Unexpected end of input: expected `(`")),
        },
        Some(token) => Err(format!("Expected `for` found: {token}")),
        None => Err(String::from("Unexpected end of input: expected `for`")),
    }
}

#[instrument]
fn consume_expr(state: ParserState) -> ParseResult<Stmt> {
    debug!("Consuming expression statment");
    let (mut state, expr) = exprs::parse(state)?;
    match state.tokens.pop_front() {
        Some(Token::Semicolon) => Ok((state, Stmt::Expr(expr))),
        Some(token) => Err(format!("Expected `;` found: {token}")),
        None => Err(String::from("Unexpected end of input: expected `;`")),
    }
}

#[instrument]
fn consume_case(mut state: ParserState) -> ParseResult<Stmt> {
    debug!("Consuming case statment");
    match state.tokens.pop_front() {
        Some(Token::Case) => {
            let (mut state, expr) = exprs::parse(state)?;
            match state.tokens.pop_front() {
                Some(Token::Colon) => {
                    let parent = state.current_switch().map(|scope| scope.label().into());
                    let (mut state, body) = parse(state)?;
                    if let None = parent {
                        let e = "Found case stmt outside switch stmt scope";
                        state.errors.push(e.into());
                    }
                    Ok((state, Stmt::Case(Clause::new(parent, expr, body))))
                }
                Some(token) => Err(format!("Expected `:` after case, found: {token}")),
                None => Err("Unexpected end of input after case: expected `:`".into()),
            }
        }
        Some(token) => Err(format!("Expected `case` found: {token}")),
        None => Err(String::from("Unexpected end of input: expected `case`")),
    }
}

#[instrument]
fn consume_default(mut state: ParserState) -> ParseResult<Stmt> {
    debug!("Consuming default statment");
    match state.tokens.pop_front() {
        Some(Token::Default) => match state.tokens.pop_front() {
            Some(Token::Colon) => {
                let parent = state.current_switch().map(|scope| scope.label().into());
                let (mut state, body) = parse(state)?;
                if let None = parent {
                    let e = "Found default stmt outside switch stmt scope";
                    state.errors.push(e.into());
                }
                Ok((state, Stmt::Default(Default::new(parent, body))))
            }
            Some(token) => Err(format!("Expected `:` after default, found: {token}")),
            None => Err("Unexpected end of input after default: expected `:`".into()),
        },
        Some(token) => Err(format!("Expected `default` found: {token}")),
        None => Err(String::from("Unexpected end of input: expected `default`")),
    }
}

#[instrument]
fn consume_switch(mut state: ParserState) -> ParseResult<Stmt> {
    debug!("Consuming switch statment");
    match state.tokens.pop_front() {
        Some(Token::Switch) => match state.tokens.pop_front() {
            Some(Token::LParen) => {
                let (mut state, expr) = exprs::parse(state)?;
                match state.tokens.pop_front() {
                    Some(Token::RParen) => {
                        let (mut state, body) = parse(state)?;
                        let id = generate_tag("switch");
                        state.scopes.push_back(Scope::Switch(id.to_string()));
                        let stmt = Stmt::Switch(Switch::new(id, expr, body));
                        state.scopes.pop_back();
                        Ok((state, stmt))
                    }
                    Some(token) => Err(format!("Expected `)` found: {token}")),
                    None => Err(String::from("Unexpected end of input: expected `)`")),
                }
            }
            Some(token) => Err(format!("Expected `(` found: {token}")),
            None => Err(String::from("Unexpected end of input: expected `(`")),
        },
        Some(token) => Err(format!("Expected `switch` found: {token}")),
        None => Err(String::from("Unexpected end of input: expected `switch`")),
    }
}

#[instrument]
fn consume_identity(state: ParserState) -> ParseResult<Stmt> {
    debug!("Consuming identity");
    match (state.tokens.get(0), state.tokens.get(1)) {
        (Some(_), Some(Token::Colon)) => consume_label(state),
        (Some(_), _) => consume_expr(state),
        _ => Err("Unexpected end of input: expected expr or label".into()),
    }
}

#[instrument]
pub fn parse(state: ParserState) -> ParseResult<Stmt> {
    debug!("Consuming statment");
    let token = state.tokens.front();
    match token.ok_or("Unexpected end of input: expected stmt")? {
        Token::Semicolon => consume_null(state),
        Token::LBrace => consume_block(state),
        Token::Return => consume_return(state),
        Token::If => consume_if(state),
        Token::While => consume_while(state),
        Token::Do => consume_dowhile(state),
        Token::For => consume_for(state),
        Token::Switch => consume_switch(state),
        Token::Case => consume_case(state),
        Token::Default => consume_default(state),
        Token::Break => consume_break(state),
        Token::Continue => consume_continue(state),
        Token::Goto => consume_goto(state),
        Token::Ident(_) => consume_identity(state),
        _ => consume_expr(state),
    }
}
