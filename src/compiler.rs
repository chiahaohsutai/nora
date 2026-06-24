use std::env::temp_dir;
use std::ffi::OsStr;
use std::path::{Path, PathBuf};
use std::process::{Command, Output};

use super::driver::CompilerStage;

mod tokens;

pub trait Executor {
    fn execute(&self, args: &[&OsStr]) -> Result<Output, String>;
}

struct GccExecutor {
    command: String,
}

impl GccExecutor {
    pub fn new(command: impl AsRef<str>) -> Self {
        Self {
            command: command.as_ref().into(),
        }
    }
}

impl Default for GccExecutor {
    fn default() -> Self {
        Self {
            command: String::from("gcc"),
        }
    }
}

impl Executor for GccExecutor {
    fn execute(&self, args: &[&OsStr]) -> Result<Output, String> {
        let mut command = Command::new(&self.command);
        command.args(args).output().map_err(|e| e.to_string())
    }
}

pub trait Toolchain {
    fn preprocess(&self, input: impl AsRef<Path>) -> Result<PathBuf, String>;
    fn link(&self, input: impl AsRef<Path>) -> Result<PathBuf, String>;
}

pub struct GccToolchain {
    executor: GccExecutor,
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
    fn preprocess(&self, input: impl AsRef<Path>) -> Result<PathBuf, String> {
        let input = input.as_ref();
        if !input.is_file() {
            let input = input.display();
            return Err(format!("Input for preprocessing must be a file: {input}"));
        }
        let mut output = input.with_extension("i");
        output = temp_dir().join(output.file_name().unwrap()).to_path_buf();

        let args: [&OsStr; 5] = [
            "-E".as_ref(),
            "-P".as_ref(),
            input.as_os_str(),
            "-o".as_ref(),
            output.as_os_str(),
        ];
        match self.executor.execute(&args) {
            Ok(out) if out.status.success() => Ok(output),
            Ok(out) => Err(format!(
                "Preprocessing failed: {}",
                String::from_utf8_lossy(&out.stderr)
            )),
            Err(err) => Err(format!("Preprocessing command failed: {err}")),
        }
    }

    fn link(&self, input: impl AsRef<Path>) -> Result<PathBuf, String> {
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

    pub fn run(input: impl AsRef<Path>, stage: Option<CompilerStage>) -> Result<(), String> {
        todo!()
    }
}
