use std::fmt;

/// Represents a lexical token produced by the lexer.
#[derive(Debug, PartialEq, Clone, Eq)]
pub enum Token {
    Identifier(String),
    Constant(u64),
    Integer,
    Void,
    Return,
    OpenParen,
    CloseParen,
    OpenBrace,
    CloseBrace,
    Semicolon,
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let token = match self {
            Self::Identifier(identifier) => identifier,
            Self::Constant(constant) => &constant.to_string(),
            Self::Integer => "int",
            Self::Void => "void",
            Self::Return => "return",
            Self::OpenParen => "(",
            Self::CloseParen => ")",
            Self::OpenBrace => "{",
            Self::CloseBrace => "}",
            Self::Semicolon => ";",
        };
        write!(f, "{token}")
    }
}
