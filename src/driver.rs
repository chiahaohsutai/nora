use std::path::PathBuf;

use clap::{Parser, ValueEnum};

#[derive(Clone, Copy, Debug, PartialEq, ValueEnum)]
pub enum CompilerStage {
    #[value(help = "Stop after the lexing stage.")]
    Lex,

    #[value(help = "Stop after the parser stage.")]
    Parse,

    #[value(help = "Stop after code generation stage.")]
    Codegen,
}

#[derive(Parser)]
pub struct CompilerDriver {
    #[arg(long, help = "Stops at the specified stage")]
    pub stage: CompilerStage,

    #[arg(help = "Path to the C source file")]
    pub path: PathBuf,
}
