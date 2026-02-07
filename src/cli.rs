use std::str::FromStr;

use clap::{ArgGroup, ArgMatches, Command, Id, arg};

use super::compiler;

pub struct Args {
    pub path: String,
    pub target: compiler::Target,
    pub stop_after: Option<compiler::Stage>,
}

impl Args {
    #[rustfmt::skip]
    fn new(path: String, target: compiler::Target, stop_after: Option<compiler::Stage>) -> Self {
        Self { path, target, stop_after }
    }
}

impl From<ArgMatches> for Args {
    fn from(value: ArgMatches) -> Self {
        let path = value.get_one::<String>("path").unwrap();
        let target = if *value.get_one("compile").unwrap_or(&false) {
            compiler::Target::ObjFile
        } else {
            compiler::Target::Execuable
        };
        let stop_after = value
            .get_one::<Id>("stop_after")
            .map(|id| compiler::Stage::from_str(id.as_str()).ok())
            .flatten();

        Args::new(String::from(path), target, stop_after)
    }
}

pub fn get_matches() -> Args {
    let args = [
        arg!(<path> "Absolute or relative path to C source file"),
        arg!(--lex "Runs the lexer and exits"),
        arg!(--parse "Runs the parser and exits"),
        arg!(--tacky "Generates tacky assembly and exits"),
        arg!(--codegen "Runs assembly generation and exits"),
        arg!(--validate "Runs semantic analysis and exists"),
        arg!("compile": -c "Generates an object file instead of an execuable"),
    ];
    let stop_after = ArgGroup::new("stop_after")
        .args(["lex", "parse", "tacky", "codegen", "validate"])
        .multiple(false)
        .required(false);

    Command::new("NORA")
        .about("A C compiler written in Rust")
        .long_about("A simple C compiler for a subset of C")
        .author("Your favorite programmer CHIA")
        .alias("nora")
        .args(args)
        .group(stop_after)
        .get_matches()
        .into()
}
