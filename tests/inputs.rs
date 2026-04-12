use std::env::temp_dir;
use std::fs::{remove_file, write};

use assert_cmd::{Command, cargo};
use predicates::str::contains;
use rand::distr::{Alphanumeric, SampleString};
use rand::rng;

#[test]
fn test_cli_returns_error_if_path_does_not_exist() {
    let path = format!("{}.c", Alphanumeric.sample_string(&mut rng(), 24));

    let mut cli = Command::new(cargo::cargo_bin!());
    cli.arg(&path);

    let error = format!("File {path} does not exist");
    cli.assert().failure().stderr(contains(error));
}

#[test]
fn test_cli_returns_error_if_path_does_not_have_c_extension() {
    let filestem = Alphanumeric.sample_string(&mut rng(), 24);
    let path = temp_dir().join(format!("{}.mock", filestem));

    write(&path, "Hello World!").unwrap();

    let mut cmd = Command::new(cargo::cargo_bin!());
    cmd.arg(&path);

    let error = format!("File must be a .c file: {}", path.display());
    cmd.assert().failure().stderr(contains(error));

    remove_file(&path).unwrap();
}
