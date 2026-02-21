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
    #[arg(long, short, help = "Tokenize the program")]
    lex: bool,

    #[arg(long, short, help = "Generate an AST for the program")]
    parse: bool,

    #[arg(long, short, help = "Run semantic analysis on the AST")]
    validate: bool,

    #[arg(long, short, help = "Generate TAC instructions")]
    tacky: bool,

    #[arg(long, short, help = "Generate assembly instructions")]
    codegen: bool,

    #[arg(help = "Generate an object file")]
    compile: bool,
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
