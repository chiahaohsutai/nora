use std::collections::VecDeque;

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

#[derive(Debug, PartialEq)]
pub enum Token {
    Ident(String),
    Const(u64),
    Paren(bool),
    Brace(bool),
    KW(Keyword),
    Semicolon,
}

enum LexState {
    Init,
    Punc,
    Ident,
    Const,
    Group,
}

pub struct Lexer {
    tokens: Vec<Token>,
    buffer: Vec<char>,
    mark: Option<usize>,
    pending: VecDeque<char>,
}

impl Default for Lexer {
    fn default() -> Self {
        Lexer {
            tokens: Vec::new(),
            buffer: Vec::new(),
            pending: VecDeque::new(),
            mark: None,
        }
    }
}

impl Lexer {
    fn on_const(&mut self, input: char) -> Result<LexState, String> {
        match input {
            '0'..='9' => {
                self.buffer.push(input);
                Ok(LexState::Const)
            }
            'a'..='z' | 'A'..='Z' => {
                let message = "Expected word boundry or number";
                Err(format!("{message}, found {input}"))
            }
            _ => {
                if !input.is_whitespace() {
                    self.pending.push_back(input);
                }
                let constant: String = self.buffer.iter().collect();
                self.buffer = Vec::new();

                self.tokens.push(Token::Const(constant.parse().unwrap()));
                Ok(LexState::Init)
            }
        }
    }

    fn on_ident(&mut self, input: char) -> Result<LexState, String> {
        match input {
            'a'..='z' | 'A'..='Z' | '0'..='9' | '_' => {
                self.buffer.push(input);
                Ok(LexState::Ident)
            }
            _ => {
                if !input.is_whitespace() {
                    self.pending.push_back(input);
                }
                let value: String = self.buffer.iter().collect();
                self.buffer = Vec::new();

                let token = match Keyword::try_from(value.as_str()) {
                    Ok(kw) => Token::KW(kw),
                    Err(_) => Token::Ident(value),
                };
                self.tokens.push(token);
                Ok(LexState::Init)
            }
        }
    }

    fn on_group(&mut self, input: char) -> Result<LexState, String> {
        match input {
            '(' | ')' => {
                self.tokens.push(Token::Paren(input == '('));
                Ok(LexState::Init)
            }
            '{' | '}' => {
                self.tokens.push(Token::Brace(input == '{'));
                Ok(LexState::Init)
            }
            _ => Err(format!("Expected grouping delimiter, found: {input}")),
        }
    }

    fn on_punc(&mut self, input: char) -> Result<LexState, String> {
        match input {
            ';' => {
                self.tokens.push(Token::Semicolon);
                Ok(LexState::Init)
            }
            _ => Err(format!("Expected punctuation delimiter, found: {input}")),
        }
    }

    fn on_init(&mut self, input: char) -> Result<LexState, String> {
        match input {
            ';' => self.on_punc(input),
            '0'..='9' => self.on_const(input),
            'a'..='z' | 'A'..='Z' => self.on_ident(input),
            '(' | ')' | '{' | '}' => self.on_group(input),
            ' ' | '\n' | '\t' | '\r' => Ok(LexState::Init),
            _ => Err(format!("Unexpected character: {input}")),
        }
    }

    fn advance(&mut self, state: LexState, input: char) -> Result<LexState, String> {
        match state {
            LexState::Init => self.on_init(input),
            LexState::Punc => self.on_punc(input),
            LexState::Const => self.on_const(input),
            LexState::Ident => self.on_ident(input),
            LexState::Group => self.on_group(input),
        }
    }

    pub fn lex<T: AsRef<str>>(mut self, input: T) -> Result<Vec<Token>, String> {
        let mut state = LexState::Init;
        let mut chars = input.as_ref().chars();

        while let Some(c) = self.pending.pop_back().or_else(|| chars.next()) {
            state = match self.advance(state, c) {
                Ok(state) => state,
                Err(e) => return Err(format!("Malformed input: {e}")),
            }
        }
        Ok(self.tokens)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn lexer_returns_error_on_invalid_ident_value() {
        let input = "int main(void)\n{\n\treturn @b;\n}";
        let result = Lexer::default().lex(input);
        let expected = "Malformed input: Unexpected character: @";
        assert!(result.is_err_and(|err| err == expected));
    }

    #[test]
    fn lexer_returns_correct_tokens_on_valid_input() {
        let input = "int main(void)\n{\n\treturn b;\n}";
        let result = Lexer::default().lex(input);
        let expected = vec![
            Token::KW(Keyword::Int),
            Token::Ident("main".into()),
            Token::Paren(true),
            Token::KW(Keyword::Void),
            Token::Paren(false),
            Token::Brace(true),
            Token::KW(Keyword::Return),
            Token::Ident("b".into()),
            Token::Semicolon,
            Token::Brace(false),
        ];
        assert!(result.is_ok_and(|tokens| tokens == expected));
    }
}
