use super::super::tokenizer::Token;
use super::{ParserResult, State, blocks, decls, exprs};

type StmtResult = ParserResult<Stmt>;

enum ForInit {
    Decl(decls::Decl),
    Expr(Option<exprs::Expr>),
}

impl From<decls::Decl> for ForInit {
    fn from(value: decls::Decl) -> Self {
        Self::Decl(value)
    }
}

impl From<exprs::Expr> for ForInit {
    fn from(value: exprs::Expr) -> Self {
        Self::Expr(Some(value))
    }
}

struct If {
    cond: exprs::Expr,
    body: Box<Self>,
    otherwise: Option<Box<Self>>,
}

struct Label {
    name: String,
    body: Box<Stmt>,
}

impl Label {
    #[rustfmt::skip]
    fn new(name: String, body: Stmt) -> Self {
        Self { name, body: Box::new(body) }
    }
}

struct While {
    id: String,
    cond: exprs::Expr,
    body: Box<Stmt>,
}

enum Case {
    Int(u64, String),
    Default(String),
}

struct Switch {
    id: String,
    value: exprs::Expr,
    body: Box<Stmt>,
    cases: Vec<Case>,
}

struct Clause {
    parent: String,
    value: exprs::Expr,
    body: Box<Stmt>,
}

struct Default {
    parent: String,
    body: Box<Stmt>,
}

struct For {
    id: String,
    init: ForInit,
    cond: Option<exprs::Expr>,
    post: Option<exprs::Expr>,
    body: Box<Stmt>,
}

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

fn consume_null(mut state: State) -> StmtResult {
    match state.tokens.pop_front() {
        Some(Token::Semicolon) => Ok((Stmt::Null, state)),
        Some(token) => Err(format!("Expected `;` found: {token}")),
        None => Err(String::from("Unexpected end of input: expected `;`")),
    }
}

fn consume_return(mut state: State) -> StmtResult {
    match state.tokens.pop_front() {
        Some(Token::Return) => {
            let (expr, mut state) = exprs::parse(state)?;
            match state.tokens.pop_front() {
                Some(Token::Semicolon) => Ok((Stmt::Return(expr), state)),
                Some(token) => return Err(format!("Expected `;` found: {token}")),
                None => return Err(String::from("Unexpected end of input: expected `;`")),
            }
        }
        Some(token) => Err(format!("Expected `return` found: {token}")),
        None => Err(String::from("Unexpected end of input: expected `return`")),
    }
}

fn consume_break(mut state: State) -> StmtResult {
    match state.tokens.pop_front() {
        Some(Token::Break) => match state.tokens.pop_front() {
            Some(Token::Semicolon) => Ok((Stmt::Break(None), state)),
            Some(token) => Err(format!("Expected `;` found: {token}")),
            None => Err(String::from("Unexpected end of input: expected `;`")),
        },
        Some(token) => Err(format!("Expected `break` found: {token}")),
        None => Err(String::from("Unexpected end of input: expected `break`")),
    }
}

fn consume_continue(mut state: State) -> StmtResult {
    match state.tokens.pop_front() {
        Some(Token::Continue) => {
            let label = state
                .current_loop()
                .map(|scope| String::from(scope.as_ref()));
            match state.tokens.pop_front() {
                Some(Token::Semicolon) => Ok((Stmt::Continue(label), state)),
                Some(token) => return Err(format!("Expected `;` found: {token}")),
                None => return Err(String::from("Unexpected end of input: expected `;`")),
            }
        }
        Some(token) => Err(format!("Expected `continue` found: {token}")),
        None => Err(String::from("Unexpected end of input: expected `continue`")),
    }
}

fn consume_label(mut state: State) -> StmtResult {
    match state.tokens.pop_front() {
        Some(Token::Ident(ident)) => match state.tokens.pop_front() {
            Some(Token::Colon) => {
                let (stmt, state) = parse(state)?;
                Ok((Stmt::Label(Label::new(ident, stmt)), state))
            }
            Some(token) => return Err(format!("Expected `:` found: {token}")),
            None => Err(String::from("Unexpected end of input: expected `:`")),
        },
        Some(token) => return Err(format!("Expected identifier found: {token}")),
        None => Err(String::from("Unexpected end of input: expected `label`")),
    }
}

fn consume_goto(mut state: State) -> StmtResult {
    match state.tokens.pop_front() {
        Some(Token::Goto) => match state.tokens.pop_front() {
            Some(Token::Ident(ident)) => match state.tokens.pop_front() {
                Some(Token::Semicolon) => Ok((Stmt::Goto(ident), state)),
                Some(token) => return Err(format!("Expected `;` found: {token}")),
                None => return Err(String::from("Unexpected end of input: expected `;`")),
            },
            Some(token) => return Err(format!("Expected identifier found: {token}")),
            None => return Err(String::from("Unexpected end of input: expected identifier")),
        },
        Some(token) => return Err(format!("Expected `goto` found: {token}")),
        None => return Err(String::from("Unexpected end of input: expected `goto`")),
    }
}

fn consume_block(mut state: State) -> StmtResult {
    match state.tokens.pop_front() {
        Some(Token::LBrace) => {
            let (block, state) = blocks::parse(state)?;
            match state.tokens.pop_front() {
                Some(Token::RBrace) => Ok((Stmt::Comp(block), state)),
                Some(token) => return Err(format!("Expected `}}` found: {token}")),
                None => return Err(String::from("Unexpected end of input: expected `}`")),
            }
        }
        Some(token) => return Err(format!("Expected `{{` found: {token}")),
        None => return Err(String::from("Unexpected end of input: expected: `{`")),
    }
}

fn consume_if(mut state: State) -> StmtResult {
    todo!()
}

pub fn parse(state: State) -> StmtResult {
    let token = state.tokens.front();
    match token.ok_or("Unexpected end of input: expected stmt")? {
        Token::Semicolon => consume_null(state),
        Token::LBrace => consume_block(state),
        Token::Return => consume_return(state),
        Token::If => consume_if(state),
        Token::Break => consume_break(state),
        Token::Continue => consume_continue(state),
        Token::Goto => consume_goto(state),
        Token::Ident(_) => consume_label(state),
        _ => todo!(),
    }
}
