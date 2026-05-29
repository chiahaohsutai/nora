use std::fs::{read_to_string, remove_file};
use std::path::Path;

use super::{Gcc, temp_file_path};

mod lexer;
mod tokens;

/// Describes the different compilation phases in a compiler.
#[derive(PartialEq, Debug)]
pub enum Phase {
    Lex,
    Parse,
    Codegen,
}

/// Describes why the compilation process exited.
#[derive(PartialEq, Debug)]
pub enum ExitReason {
    Completed,
    StoppedAfter(Phase),
}

/// Runs the compilation pipeline on the given `.c` input file.
pub fn compile(path: &Path, stop_after: Option<Phase>) -> Result<ExitReason, String> {
    let gcc = Gcc::default();
    let inter = temp_file_path(Some("i"), 24);

    if let Err(err) = gcc.preprocess(path, &inter) {
        return Err(format!("Failed to preprocess input: {err}"));
    };
    let _ = match read_to_string(&inter) {
        Ok(contents) => match lexer::Lexer::default().lex(contents) {
            Ok(tokens) => tokens,
            Err(err) => {
                let _ = remove_file(&inter);
                return Err(format!("Failed to open file: {err}"));
            }
        },
        Err(err) => {
            let _ = remove_file(&inter);
            return Err(format!("Failed to open file: {err}"));
        }
    };

    let _ = remove_file(inter);
    if stop_after.is_some_and(|sa| sa == Phase::Lex) {
        return Ok(ExitReason::StoppedAfter(Phase::Lex));
    }
    Ok(ExitReason::Completed)
}
