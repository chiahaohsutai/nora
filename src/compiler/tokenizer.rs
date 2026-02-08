use std::fmt;

use regex::Regex;

#[rustfmt::skip]
const KWS: &str = r"\b(?:continue|switch|default|return|while|break|void|case|else|goto|for|int|if|do)\b";
const OPS: &str = r"(?:<<=|>>=|\+=|-=|/=|\*=|%=|&=|\|=|\^=|<=|>=|--|\+\+|<<|>>|&&|\|\||==|!=|-|~|\+|\*|/|%|&|\||\^|!|>|<|=)";
const DELIMS: &str = r"(?:,|;|:|\?|\(|\)|\{|\})";
const IDENTS: &str = r"(?:[a-zA-Z_]\w*)\b";
const CONSTS: &str = r"(?:[0-9]+)\b";

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Token {
    PlusPlus,      // ++
    MinusMinus,    // --
    Bang,          // !
    Tilde,         // ~
    Plus,          // +
    Minus,         // -
    Star,          // *
    Slash,         // /
    Percent,       // %
    Amp,           // &
    Pipe,          // |
    Caret,         // ^
    Shl,           // <<
    Shr,           // >>
    AmpAmp,        // &&
    PipePipe,      // ||
    EqEq,          // ==
    NotEq,         // !=
    Lt,            // <
    Gt,            // >
    Le,            // <=
    Ge,            // >=
    Eq,            // =
    PlusEq,        // +=
    MinusEq,       // -=
    StarEq,        // *=
    SlashEq,       // /=
    PercentEq,     // %=
    AmpEq,         // &=
    PipeEq,        // |=
    CaretEq,       // ^=
    ShlEq,         // <<=
    ShrEq,         // >>=
    LParen,        // (
    RParen,        // )
    LBrace,        // {
    RBrace,        // }
    Semicolon,     // ;
    Colon,         // :
    Eroteme,       // ?
    Comma,         // ,
    If,            // if
    Else,          // else
    Int,           // int
    Void,          // void
    Return,        // return
    Goto,          // goto
    Do,            // do
    While,         // while
    For,           // for
    Break,         // break
    Continue,      // continue
    Switch,        // switch
    Case,          // case
    Default,       // default
    Const(u64),    // constant
    Ident(String), // identifier
}

impl From<&str> for Token {
    fn from(s: &str) -> Self {
        match s {
            "++" => Self::PlusPlus,
            "--" => Self::MinusMinus,
            "!" => Self::Bang,
            "~" => Self::Tilde,
            "+" => Self::Plus,
            "-" => Self::Minus,
            "*" => Self::Star,
            "/" => Self::Slash,
            "%" => Self::Percent,
            "&" => Self::Amp,
            "|" => Self::Pipe,
            "^" => Self::Caret,
            "<<" => Self::Shl,
            ">>" => Self::Shr,
            "&&" => Self::AmpAmp,
            "||" => Self::PipePipe,
            "==" => Self::EqEq,
            "!=" => Self::NotEq,
            "<" => Self::Lt,
            ">" => Self::Gt,
            "<=" => Self::Le,
            ">=" => Self::Ge,
            "=" => Self::Eq,
            "+=" => Self::PlusEq,
            "-=" => Self::MinusEq,
            "*=" => Self::StarEq,
            "/=" => Self::SlashEq,
            "%=" => Self::PercentEq,
            "&=" => Self::AmpEq,
            "|=" => Self::PipeEq,
            "^=" => Self::CaretEq,
            "<<=" => Self::ShlEq,
            ">>=" => Self::ShrEq,
            "(" => Self::LParen,
            ")" => Self::RParen,
            "{" => Self::LBrace,
            "}" => Self::RBrace,
            ";" => Self::Semicolon,
            ":" => Self::Colon,
            "?" => Self::Eroteme,
            "," => Self::Comma,
            "if" => Self::If,
            "else" => Self::Else,
            "int" => Self::Int,
            "void" => Self::Void,
            "return" => Self::Return,
            "Goto" => Self::Goto,
            "Do" => Self::Do,
            "While" => Self::While,
            "For" => Self::For,
            "Break" => Self::Break,
            "Continue" => Self::Continue,
            "Switch" => Self::Switch,
            "Case" => Self::Case,
            "Default" => Self::Default,
            s => match s.parse::<u64>() {
                Ok(constant) => Self::Const(constant),
                Err(_) => Self::Ident(String::from(s)),
            },
        }
    }
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let s = match self {
            Self::PlusPlus => "++",
            Self::MinusMinus => "--",
            Self::Bang => "!",
            Self::Tilde => "~",
            Self::Plus => "+",
            Self::Minus => "-",
            Self::Star => "*",
            Self::Slash => "/",
            Self::Percent => "%",
            Self::Amp => "&",
            Self::Pipe => "|",
            Self::Caret => "^",
            Self::Shl => "<<",
            Self::Shr => ">>",
            Self::AmpAmp => "&&",
            Self::PipePipe => "||",
            Self::EqEq => "==",
            Self::NotEq => "!=",
            Self::Lt => "<",
            Self::Gt => ">",
            Self::Le => "<=",
            Self::Ge => ">=",
            Self::Eq => "=",
            Self::PlusEq => "+=",
            Self::MinusEq => "-=",
            Self::StarEq => "*=",
            Self::SlashEq => "/=",
            Self::PercentEq => "%=",
            Self::AmpEq => "&=",
            Self::PipeEq => "|=",
            Self::CaretEq => "^=",
            Self::ShlEq => "<<=",
            Self::ShrEq => ">>=",
            Self::LParen => "(",
            Self::RParen => ")",
            Self::LBrace => "{",
            Self::RBrace => "}",
            Self::Semicolon => ";",
            Self::Colon => ":",
            Self::Eroteme => "?",
            Self::Comma => ",",
            Self::If => "if",
            Self::Else => "else",
            Self::Int => "int",
            Self::Void => "void",
            Self::Return => "return",
            Self::Goto => "Goto",
            Self::Do => "Do",
            Self::While => "While",
            Self::For => "For",
            Self::Break => "Break",
            Self::Continue => "Continue",
            Self::Switch => "Switch",
            Self::Case => "Case",
            Self::Default => "Default",
            Self::Const(val) => return write!(f, "{}", val),
            Self::Ident(name) => return write!(f, "{}", name),
        };
        write!(f, "{}", s)
    }
}

pub fn tokenize<T: AsRef<str>>(input: T) -> Result<Vec<Token>, String> {
    let pattern = format!(r"^(?:(?:\s+)|(?:{KWS}|{IDENTS}|{CONSTS}|{OPS}|{DELIMS}))");
    let re = Regex::new(&pattern).unwrap();
    
    let mut tokens = Vec::new();
    let mut i = 0;

    while i < input.as_ref().len() {
        let rest = &input.as_ref()[i..];
        if let Some(capture) = re.find(rest) {
            let capture = capture.as_str();
            (!capture.trim().is_empty()).then(|| tokens.push(Token::from(capture)));
            i += capture.len()
        } else {
            return Err(format!("Invalid token at index {i}"));
        }
    }
    Ok(tokens)
}
