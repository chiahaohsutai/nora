use std::ffi::OsStr;
use std::path::PathBuf;

use clap::{Args, Parser};

use nora::{Phase, compile};

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

#[derive(Parser, Debug)]
#[command(about, version)]
struct Cli {
    #[arg(help = "Path to C source file")]
    path: PathBuf,

    #[command(flatten)]
    stage: Stage,
}

fn main() -> Result<(), String> {
    let cli = Cli::parse();

    if !cli.path.exists() {
        return Err(format!("File {} does not exist", cli.path.display()));
    }
    if cli.path.extension().unwrap_or(OsStr::new("")) != "c" {
        return Err(format!("File must be a .c file: {}", cli.path.display()));
    }

    let stop_after = if cli.stage.lex {
        Some(Phase::Lex)
    } else if cli.stage.parse {
        Some(Phase::Parse)
    } else if cli.stage.codegen {
        Some(Phase::Codegen)
    } else {
        None
    };

    Ok(())
}
