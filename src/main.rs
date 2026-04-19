use std::ffi::OsStr;
use std::fs::{File, remove_file};
use std::io::{BufRead, BufReader};
use std::path::{Path, PathBuf};
use std::process::{ExitCode, Termination};

use clap::{Args, Parser};

use nora::{Gcc, Lexer, Luthor, Preprocessor, random_temp_file_path};

#[derive(PartialEq, Debug)]
enum Phase {
    Lex,
    Parse,
    Codegen,
}

#[derive(PartialEq, Debug)]
enum ExitReason {
    Completed,
    StoppedAfter(Phase),
}

impl Termination for ExitReason {
    fn report(self) -> ExitCode {
        ExitCode::SUCCESS
    }
}

#[derive(Args, Debug)]
#[group(multiple = false)]
struct Stage {
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
struct Cli {
    #[arg(help = "Path to C source file")]
    path: PathBuf,

    #[command(flatten)]
    stage: Stage,
}

fn compile<T>(path: &Path, lexer: T, stop_after: Option<Phase>) -> Result<ExitReason, String>
where
    T: Lexer,
{
    let gcc = Gcc::default();
    let out = random_temp_file_path(Some("i"), 24);

    if let Err(err) = gcc.preprocess(path, &out) {
        return Err(format!("Failed to preprocess input: {err}"));
    };

    let intermediate = match File::open(&out) {
        Ok(file) => file,
        Err(err) => {
            let _ = remove_file(&out);
            return Err(format!("Failed to open file: {err}"));
        }
    };
    let reader = BufReader::new(intermediate);
    let lines = reader.lines();

    let chars = lines.flat_map(|line| match line {
        Ok(l) => l.chars().map(Ok).collect::<Vec<_>>(),
        Err(e) => vec![Err(e.to_string())],
    });

    let _ = match lexer.lex(chars) {
        Ok(tokens) => tokens,
        Err(err) => {
            let _ = remove_file(out);
            return Err(format!("Lexing failed: {err}"));
        }
    };

    let _ = remove_file(out);
    if stop_after.is_some_and(|sa| sa == Phase::Lex) {
        return Ok(ExitReason::StoppedAfter(Phase::Lex));
    }

    Ok(ExitReason::Completed)
}

fn main() -> Result<ExitReason, String> {
    let cli = Cli::parse();

    if !cli.path.exists() {
        return Err(format!("File {} does not exist", cli.path.display()));
    }
    if cli.path.extension().unwrap_or(OsStr::new("")) != "c" {
        return Err(format!("File must be a .c file: {}", cli.path.display()));
    }

    let stop_after = Option::<Phase>::from(cli.stage);
    let lexer = Luthor;

    compile(&cli.path, lexer, stop_after)
}

#[cfg(test)]
mod tests {
    use std::fs::write;

    use nora::Token;

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

    struct MockLexer;

    impl Lexer for MockLexer {
        fn lex<T>(&self, _: T) -> Result<Vec<Token>, String>
        where
            T: Iterator<Item = Result<char, String>>,
        {
            Ok(Vec::new())
        }
    }

    #[test]
    fn program_exists_after_lexing() {
        let path = random_temp_file_path(Some("c"), 24);
        write(&path, "int main(){return 0;}").unwrap();

        let result = compile(&path, MockLexer, Some(Phase::Lex));
        assert_eq!(result, Ok(ExitReason::StoppedAfter(Phase::Lex)));
    }
}
