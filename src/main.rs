use clap::{Args, Parser};
use std::path::PathBuf;

#[derive(Parser, Debug)]
#[command(about, version)]
struct Cli {
    #[arg(help = "Path to C source file")]
    path: PathBuf,

    #[command(flatten)]
    stage: Stages,
}

#[derive(Args, Debug)]
#[group(multiple = false)]
struct Stages {
    #[arg(long, help = "Tokenize the program")]
    lex: bool,

    #[arg(long, help = "Generate an AST for the program")]
    parse: bool,

    #[arg(long, help = "Generate assembly instructions")]
    codegen: bool,
}

fn main() -> Result<(), String> {
    let cli = Cli::parse();

    if !cli.path.exists() {
        return Err(format!("File {} does not exist", cli.path.display()));
    }
    if !cli.path.exists() || !cli.path.extension().is_some_and(|ext| ext == "c") {
        return Err(format!("File must be a .c file: {}", cli.path.display()));
    }

    Ok(())
}
