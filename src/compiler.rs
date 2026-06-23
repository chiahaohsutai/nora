use std::{path, process::Output};

use super::driver::CompilerStage;

mod tokens;

pub trait Executor {
    fn execute(&self, args: &[&str]) -> Result<Output, String>;
}

struct GccExecutor {
    command: String,
}

impl GccExecutor {
    pub fn new(command: impl AsRef<str>) -> Self {
        Self { command: command.as_ref().into() }
    }
}

impl Default for GccExecutor {
    fn default() -> Self {
        Self { command: String::from("gcc") }
    }
}

impl Executor for GccExecutor {
    fn execute(&self, args: &[&str]) -> Result<Output, String> {
        todo!()
    }
}

pub trait Toolchain {
    fn preprocess(&self, input: impl AsRef<path::Path>) -> Result<path::PathBuf, String>;
    fn link(&self, input: impl AsRef<path::Path>) -> Result<path::PathBuf, String>;
}

pub struct GccToolchain {
    executor: GccExecutor
}

impl GccToolchain {
    pub fn new(command: impl AsRef<str>) -> Self {
        let executor = GccExecutor::new(command);
        GccToolchain { executor }
    }
}

impl Default for GccToolchain {
    fn default() -> Self {
        Self::new("gcc")
    }
}

impl Toolchain for GccToolchain {
    fn preprocess(&self, input: impl AsRef<path::Path>) -> Result<path::PathBuf, String> {
        todo!()
    }

    fn link(&self, input: impl AsRef<path::Path>) -> Result<path::PathBuf, String> {
        todo!()
    }
}

pub struct Compiler<T: Toolchain> {
    toolchain: T,
}

impl<T: Toolchain> Compiler<T> {
    pub fn new(toolchain: T) -> Self {
        Self { toolchain }
    }

    pub fn run(input: impl AsRef<path::Path>, stage: Option<CompilerStage>) -> Result<(), String> {
        todo!()
    }
}
