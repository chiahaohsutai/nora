use std::path::Path;
use std::process::Command;

mod lexer;
mod tokens;

pub trait Toolchain {
    fn preprocess(&self, input: &Path, output: &Path) -> Result<(), String>;
    fn assemble_and_link(&self, input: &Path, output: &Path) -> Result<(), String>;
}

#[derive(Debug)]
pub struct GccToolchain {
    program: String
}

impl GccToolchain {
    pub fn new(program: &str) -> Self {
        GccToolchain { program: program.to_string() }
    }
}

impl Default for GccToolchain {
    fn default() -> Self {
        GccToolchain { program: String::from("gcc") }
    }
}

impl Toolchain for GccToolchain {
    fn preprocess(&self, input: &Path, output: &Path) -> Result<(), String> {
        if !input.is_file() || input.extension().is_none_or(|e| e != "c") {
            let path = input.display();
            return Err(format!("Invalid C program file: {path}"));
        }
        let mut command =  Command::new(&self.program);
        match command.arg("-E").arg("-P").arg(input).arg("-o").arg(output).status() {
            Ok(status) if status.success() => Ok(()),
            Ok(status) => Err(format!("Preprocessing failed with status: {status}")),
            Err(error) => Err(format!("Preprocessing command failed: {error}")),
        }
    }

    fn assemble_and_link(&self, input: &Path, output: &Path) -> Result<(), String> {
        if !input.is_file() || input.extension().is_none_or(|e| e != "s") {
            let path = input.display();
            return Err(format!("Invalid assembly file: {path}"));
        }
        let mut command =  Command::new(&self.program);
        match command.arg(input).arg("-o").arg(output).status() {
            Ok(status) if status.success() => Ok(()),
            Ok(status) => Err(format!("Linking failed with status: {status}")),
            Err(error) => Err(format!("Linking command failed: {error}")),
        }
    }
}
