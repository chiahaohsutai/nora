use super::super::{generate_tag, tokenizer::Token};
use super::{ParseResult, ParserState, Scope, blocks, decls, exprs};

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

impl While {
    #[rustfmt::skip]
    fn new(id: &str, cond: exprs::Expr, body: Stmt) -> Self {
        Self { id: id.into(), cond, body: Box::new(body) }
    }
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

fn consume_null(mut state: ParserState) -> ParseResult<Stmt> {
    match state.tokens.pop_front() {
        Some(Token::Semicolon) => Ok((state, Stmt::Null)),
        Some(token) => Err(format!("Expected `;` found: {token}")),
        None => Err(String::from("Unexpected end of input: expected `;`")),
    }
}

fn consume_return(mut state: ParserState) -> ParseResult<Stmt> {
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

fn consume_break(mut state: ParserState) -> ParseResult<Stmt> {
    match state.tokens.pop_front() {
        Some(Token::Break) => {
            let label = state.scopes.back().map(|scope| scope.label().into());
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

fn consume_continue(mut state: ParserState) -> ParseResult<Stmt> {
    match state.tokens.pop_front() {
        Some(Token::Continue) => {
            let label = state.current_loop().map(|scope| scope.label().into());
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

fn consume_label(mut state: ParserState) -> ParseResult<Stmt> {
    match state.tokens.pop_front() {
        Some(Token::Ident(ident)) => match state.tokens.pop_front() {
            Some(Token::Colon) => {
                let (mut state, stmt) = parse(state)?;
                state.labels.push_back(ident.clone());
                Ok((state, Stmt::Label(Label::new(ident, stmt))))
            }
            Some(token) => Err(format!("Expected `:` found: {token}")),
            None => Err(String::from("Unexpected end of input: expected `:`")),
        },
        Some(token) => Err(format!("Expected identifier found: {token}")),
        None => Err(String::from("Unexpected end of input: expected identifier")),
    }
}

fn consume_goto(mut state: ParserState) -> ParseResult<Stmt> {
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

fn consume_block(mut state: ParserState) -> ParseResult<Stmt> {
    match state.tokens.pop_front() {
        Some(Token::LBrace) => {
            let (mut state, block) = blocks::parse(state)?;
            match state.tokens.pop_front() {
                Some(Token::RBrace) => Ok((state, Stmt::Comp(block))),
                Some(token) => Err(format!("Expected `}}` found: {token}")),
                None => Err(String::from("Unexpected end of input: expected `}`")),
            }
        }
        Some(token) => Err(format!("Expected `{{` found: {token}")),
        None => Err(String::from("Unexpected end of input: expected `{`")),
    }
}

fn consume_cond(mut state: ParserState) -> ParseResult<exprs::Expr> {
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

fn consume_if(mut state: ParserState) -> ParseResult<Stmt> {
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

fn consume_while(mut state: ParserState) -> ParseResult<Stmt> {
    match state.tokens.pop_front() {
        Some(Token::While) => {
            let (state, cond) = consume_cond(state)?;
            let (mut state, body) = parse(state)?;
            let id = generate_tag("while.loop");
            state.scopes.push_back(Scope::Loop(id.to_string()));
            Ok((state, Stmt::While(While::new(&id, cond, body))))
        }
        Some(token) => Err(format!("Expected `while` found: {token}")),
        None => Err(String::from("Unexpected end of input: expected `while`")),
    }
}

fn consume_dowhile(mut state: ParserState) -> ParseResult<Stmt> {
    match state.tokens.pop_front() {
        Some(Token::Do) => {
            let (mut state, body) = parse(state)?;
            match state.tokens.pop_front() {
                Some(Token::While) => {
                    let (mut state, cond) = consume_cond(state)?;
                    let id = generate_tag("do.while.loop");
                    state.scopes.push_back(Scope::Loop(id.to_string()));
                    Ok((state, Stmt::DoWhile(While::new(&id, cond, body))))
                }
                Some(token) => Err(format!("Expected `while` found: {token}")),
                None => Err(String::from("Unexpected end of input: expected `while`")),
            }
        }
        Some(token) => Err(format!("Expected `do` found: {token}")),
        None => Err(String::from("Unexpected end of input: expected `do`")),
    }
}



fn consume_expr(state: ParserState) -> ParseResult<Stmt> {
    let (mut state, expr) = exprs::parse(state)?;
    match state.tokens.pop_front() {
        Some(Token::Semicolon) => Ok((state, Stmt::Expr(expr))),
        Some(token) => Err(format!("Expected `;` found: {token}")),
        None => Err(String::from("Unexpected end of input: expected `;`")),
    }
}

pub fn parse(state: ParserState) -> ParseResult<Stmt> {
    let token = state.tokens.front();
    match token.ok_or("Unexpected end of input: expected stmt")? {
        Token::Semicolon => consume_null(state),
        Token::LBrace => consume_block(state),
        Token::Return => consume_return(state),
        Token::If => consume_if(state),
        Token::While => consume_while(state),
        Token::Do => consume_dowhile(state),
        Token::For => todo!(),
        Token::Switch => todo!(),
        Token::Case => todo!(),
        Token::Default => todo!(),
        Token::Break => consume_break(state),
        Token::Continue => consume_continue(state),
        Token::Goto => consume_goto(state),
        Token::Ident(_) => consume_label(state),
        _ => consume_expr(state),
    }
}
