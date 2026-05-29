/// Represents a keyword token in a C program.
#[derive(Debug, PartialEq)]
pub enum Keyword {
    Int,
    Void,
    Return,
}

impl TryFrom<&str> for Keyword {
    type Error = String;

    fn try_from(value: &str) -> Result<Self, Self::Error> {
        match value {
            "int" => Ok(Self::Int),
            "void" => Ok(Self::Void),
            "return" => Ok(Self::Return),
            _ => Err(format!("Invalid keyword: {value}")),
        }
    }
}

/// Represents a token for a C program.
#[derive(Debug, PartialEq)]
pub enum Token {
    Ident(String),
    Const(u64),
    Paren(bool),
    Brace(bool),
    KW(Keyword),
    Semicolon,
}
