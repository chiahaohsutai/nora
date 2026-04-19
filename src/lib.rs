use std::env::temp_dir;
use std::path::{Path, PathBuf};
use std::process::Command;

use rand::distr::{Alphanumeric, SampleString};
use rand::rng;

mod tokens;

pub use tokens::Token;

pub fn random_temp_file_path<T: AsRef<str>>(ext: Option<T>, len: usize) -> PathBuf {
    let stem = Alphanumeric.sample_string(&mut rng(), len);
    let name = match ext {
        Some(ext) => format!("{stem}.{}", ext.as_ref()),
        None => stem,
    };
    temp_dir().join(PathBuf::from(name))
}

fn command_output(mut cmd: Command) -> Result<(), String> {
    match cmd.output() {
        Ok(o) if o.status.success() => Ok(()),
        Ok(o) => {
            let stderr = String::from_utf8_lossy(&o.stderr).to_string();
            Err(format!("Command failed with error: {stderr}"))
        }
        Err(e) => {
            let error = e.to_string();
            Err(format!("Failed to invoke command: {error}"))
        }
    }
}

pub struct Gcc {
    command: String,
}

impl Gcc {
    pub fn new(command: String) -> Self {
        Gcc { command }
    }

    fn preprocess_command(&self, input: &Path, output: &Path) -> Command {
        let mut cmd = Command::new(&self.command);
        cmd.arg("-E").arg("-P").arg(input).arg("-o").arg(output);
        cmd
    }

    fn link_command(&self, input: &Path, output: &Path) -> Command {
        let mut cmd = Command::new(&self.command);
        cmd.arg(input).arg("-o").arg(output);
        cmd
    }
}

impl Default for Gcc {
    fn default() -> Self {
        Gcc {
            command: String::from("gcc"),
        }
    }
}

pub trait Preprocessor {
    fn preprocess(&self, input: &Path, output: &Path) -> Result<PathBuf, String>;
}

impl Preprocessor for Gcc {
    fn preprocess(&self, input: &Path, output: &Path) -> Result<PathBuf, String> {
        if input.extension().is_some_and(|e| e != "c") {
            let input = input.display();
            Err(format!("Input path must have a .c extension: {input}"))
        } else if output.extension().is_some_and(|e| e == "i") {
            let cmd = self.preprocess_command(input, output);
            command_output(cmd).map(|_| output.to_path_buf())
        } else {
            let output = output.display();
            Err(format!("Output path must have a .i extension: {output}"))
        }
    }
}

pub trait Linker {
    fn link(&self, input: &Path, output: &Path) -> Result<PathBuf, String>;
}

impl Linker for Gcc {
    fn link(&self, input: &Path, output: &Path) -> Result<PathBuf, String> {
        if input.extension().is_some_and(|e| e != "s" && e != "o") {
            let inp = input.display();
            Err(format!("Input path must have a .s or .o extension: {inp}"))
        } else if output.extension().is_none() {
            let cmd = self.link_command(input, output);
            command_output(cmd).map(|_| output.to_path_buf())
        } else {
            let out = output.display();
            Err(format!("Output path should not have an extension: {out}"))
        }
    }
}

pub trait Lexer {
    fn lex<T>(&self, input: T) -> Result<Vec<tokens::Token>, String>
    where
        T: Iterator<Item = Result<char, String>>;
}

pub struct Luthor;

impl Lexer for Luthor {
    fn lex<T>(&self, input: T) -> Result<Vec<tokens::Token>, String>
    where
        T: Iterator<Item = Result<char, String>>,
    {
        Ok(Vec::new())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn random_temp_path_has_temp_directory_as_parent() {
        let path = random_temp_file_path(Some("mock"), 10);
        assert!(path.parent().is_some_and(|parent| parent == temp_dir()));
    }

    #[test]
    fn random_temp_path_has_correct_extension_and_size() {
        let path = random_temp_file_path(Some("mock"), 24);
        let name = path.file_name().map(|os| os.to_str()).flatten();

        assert!(name.is_some_and(|s| s.ends_with(".mock")));
        assert!(name.is_some_and(|s| s.len() == 29));
    }

    #[test]
    fn gcc_preprocessor_builds_correct_preprocessing_command() {
        let pre = Gcc::default();
        let input = Path::new("input.c");
        let output = Path::new("output.i");

        let cmd = pre.preprocess_command(input, output);
        let args: Vec<_> = cmd.get_args().collect();

        assert_eq!(args, &["-E", "-P", "input.c", "-o", "output.i"])
    }

    #[test]
    fn gcc_linker_builds_correct_linking_command() {
        let linker = Gcc::default();
        let input = Path::new("input.s");
        let output = Path::new("output");

        let cmd = linker.link_command(input, output);
        let args: Vec<_> = cmd.get_args().collect();

        assert_eq!(args, &["input.s", "-o", "output"])
    }
}
