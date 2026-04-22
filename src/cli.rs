use std::path::PathBuf;

use clap::{Args, Parser};

use super::Phase;

#[derive(Args, Debug)]
#[group(multiple = false)]
pub struct Stage {
    #[arg(long, help = "Tokenize the program")]
    lex: bool,

    #[arg(long, help = "Generate an AST for the program")]
    parse: bool,

    #[arg(long, help = "Generate assembly instructions")]
    codegen: bool,
}

impl From<Stage> for Option<Phase> {
    fn from(value: Stage) -> Self {
        if value.lex {
            Some(Phase::Lex)
        } else if value.parse {
            Some(Phase::Parse)
        } else if value.codegen {
            Some(Phase::Codegen)
        } else {
            None
        }
    }
}

#[derive(Parser, Debug)]
#[command(about, version)]
pub struct Cli {
    #[arg(help = "Path to C source file")]
    pub path: PathBuf,

    #[command(flatten)]
    pub stage: Stage,
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn lex_stage_is_converted_to_lex_phase() {
        let stage = Stage {
            lex: true,
            parse: false,
            codegen: false,
        };
        let phase = Option::<Phase>::from(stage);
        assert_eq!(phase, Some(Phase::Lex));
    }

    #[test]
    fn parse_stage_is_converted_to_parse_phase() {
        let stage = Stage {
            lex: false,
            parse: true,
            codegen: false,
        };
        let phase = Option::<Phase>::from(stage);
        assert_eq!(phase, Some(Phase::Parse));
    }

    #[test]
    fn codegen_stage_is_converted_to_codegen_phase() {
        let stage = Stage {
            lex: false,
            parse: false,
            codegen: true,
        };
        let phase = Option::<Phase>::from(stage);
        assert_eq!(phase, Some(Phase::Codegen));
    }
}
