use nanoid::nanoid;
use nanoid_dictionary::ALPHANUMERIC;
use std::str::FromStr;

mod parser;
mod tokenizer;

fn generate_tag<T: AsRef<str>>(prefix: T) -> String {
    format!("{}.{}", prefix.as_ref(), nanoid!(21, ALPHANUMERIC))
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Target {
    Execuable,
    ObjFile,
}

#[derive(Debug, Clone, PartialEq, Copy)]
pub enum Stage {
    Lex,
    Parse,
    Validate,
    Tacky,
    Codegen,
}

impl FromStr for Stage {
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "lex" => Ok(Self::Lex),
            "parse" => Ok(Self::Parse),
            "tacky" => Ok(Self::Tacky),
            "codegen" => Ok(Self::Codegen),
            "validate" => Ok(Self::Validate),
            stage => Err(format!("Invalid compiler stage: {stage}")),
        }
    }
}

type AssemblyResult = Result<Option<String>, String>;

pub fn assemble<T: AsRef<str>>(input: T, stop_after: Option<Stage>) -> AssemblyResult {
    let tokens = tokenizer::tokenize(input)?;
    if matches!(stop_after, Some(Stage::Lex)) {
        return Ok(None);
    }
    let _ = parser::parse(tokens, stop_after.ne(&Some(Stage::Parse)))?;
    if matches!(stop_after, Some(Stage::Parse)) {
        return Ok(None);
    }
    todo!()
}
