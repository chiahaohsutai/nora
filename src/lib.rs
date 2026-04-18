use std::path::{Path, PathBuf};
use std::process::Command;

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

pub trait Preprocessor {
    fn preprocess(&self, input: &Path, output: &Path) -> Result<PathBuf, String>;
}

pub trait Linker {
    fn link(&self, input: &Path, output: &Path) -> Result<PathBuf, String>;
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

impl Linker for Gcc {
    fn link(&self, input: &Path, output: &Path) -> Result<PathBuf, String> {
        if input.extension().is_some_and(|e| e != "s") {
            let input = input.display();
            Err(format!("Input path must have a .s extension: {input}"))
        } else if output.extension().is_none() {
            let cmd = self.link_command(input, output);
            command_output(cmd).map(|_| output.to_path_buf())
        } else {
            let output = output.display();
            Err(format!("Output path has invalid extension: {output}"))
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

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
