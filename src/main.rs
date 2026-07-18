use std::path::PathBuf;

use clap::{Args, Parser};

#[derive(Args, Debug)]
#[group(required = false, multiple = false)]
struct Stage {
    #[arg(short, long, help = "Execute the lexing stage and exit.")]
    lex: bool,

    #[arg(short, long, help = "Execute the parsing stage and exit.")]
    parse: bool,

    #[arg(short, long, help = "Execute the code generation stage and exit.")]
    codegen: bool,
}

#[derive(Parser, Debug)]
#[command(version, about, long_about = "Compiles a C program into an executable binary.")]
struct Driver {
    #[arg(help = "Absolute or relative path to the C program.")]
    path: PathBuf,

    #[command(flatten)]
    stage: Stage,
}

fn main() -> Result<(), String> {
    let args = Driver::parse();

    if !args.path.is_file() {
        let path = args.path.display();
        return Err(format!("Invalid file path: {path}"));
    }
    if args.path.extension().is_none_or(|e| e != "c") {
        let path = args.path.display();
        return Err(format!("Input must have a .c extension: {path}"));
    }

    todo!()
}
