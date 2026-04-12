use std::env::temp_dir;
use std::path::{Path, PathBuf};
use std::process::Command;

pub trait Preprocessor {
    fn preprocess(&self, input: &Path) -> Result<PathBuf, String>;
}

pub struct GccPreprocessor {
    command: String,
}

impl GccPreprocessor {
    pub fn new(command: String) -> Self {
        GccPreprocessor { command }
    }

    fn preprocess_command(&self, input: &Path, output: &Path) -> Command {
        let mut cmd = Command::new(&self.command);
        cmd.arg("-E").arg("-P").arg(input).arg("-o").arg(output);
        cmd
    }
}

impl Default for GccPreprocessor {
    fn default() -> Self {
        GccPreprocessor {
            command: String::from("gcc"),
        }
    }
}

impl Preprocessor for GccPreprocessor {
    fn preprocess(&self, input: &Path) -> Result<PathBuf, String> {
        let mut output = temp_dir().join(input.to_path_buf());
        output.set_extension("i");

        let mut cmd = self.preprocess_command(input, &output);
        match cmd.output() {
            Err(err) => Err(format!("Failed to execute command: {}", err.to_string())),
            Ok(out) if out.status.success() => Ok(output),
            Ok(out) => {
                let stderr = String::from_utf8_lossy(&out.stderr).to_string();
                Err(format!("Preprocessing failed: {stderr}"))
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn gcc_preprocessor_builds_correct_preprocessing_command() {
        let pre = GccPreprocessor::default();
        let input = Path::new("input.c");
        let output = Path::new("output.c");

        let cmd = pre.preprocess_command(input, output);
        let args: Vec<_> = cmd.get_args().collect();

        assert_eq!(args, &["-E", "-P", "input.c", "-o", "output.c"])
    }
}
