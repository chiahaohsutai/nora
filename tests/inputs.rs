use std::fs::{remove_file, write};

use assert_cmd::{Command, cargo};
use predicates::str::contains;

use nora::random_temp_file_path;

#[test]
fn cli_returns_error_if_path_does_not_exist() {
    let path = random_temp_file_path(Some("mock"), 24);

    let mut cli = Command::new(cargo::cargo_bin!());
    cli.arg(&path);

    let error = format!("File {} does not exist", path.display());
    cli.assert().failure().stderr(contains(error));
}

#[test]
fn cli_returns_error_if_path_does_not_have_c_extension() {
    let path = random_temp_file_path(Some("mock"), 24);
    write(&path, "Hello World!").unwrap();

    let mut cmd = Command::new(cargo::cargo_bin!());
    cmd.arg(&path);

    let error = format!("File must be a .c file: {}", path.display());
    cmd.assert().failure().stderr(contains(error));

    remove_file(&path).unwrap();
}

#[test]
fn cli_lex_flag_succeeds() {
    let path = random_temp_file_path(Some("c"), 24);
    write(&path, "int main(){return 0;}").unwrap();

    let mut cmd = Command::new(cargo::cargo_bin!());
    cmd.arg(&path).arg("--lex").assert().success();

    remove_file(&path).unwrap();
}
