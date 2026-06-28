use std::error::Error;
use std::fs::write;

use assert_cmd::Command;
use predicates::str::contains;
use assert_fs::TempDir;

#[test]
fn returns_error_on_invalid_file_path() -> Result<(), Box<dyn Error>> {
    let input = String::from("invalid_file_path.c");
    let mut cmd = Command::cargo_bin("nora")?;

    let assertion = cmd.arg(input).assert().failure();
    assertion.stderr(contains("Invalid file path"));

    Ok(())
}

#[test]
fn returns_error_on_invalid_file_extension() -> Result<(), Box<dyn Error>> {
    let tempdir = TempDir::new()?;
    let input = tempdir.path().join("program.mock");

    write(&input, "int main() { return 0; }")?;
    let mut cmd = Command::cargo_bin("nora")?;

    let assertion = cmd.arg(input).assert().failure();
    assertion.stderr(contains("Input must have a .c extension"));

    Ok(())
}
