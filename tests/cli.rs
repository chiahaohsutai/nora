use assert_cmd::{Command, cargo};
use fake::{Fake, faker::lorem};
use predicates::str::contains;
use tempfile::NamedTempFile;

#[test]
fn test_returns_error_if_path_does_not_exist() {
    let path = format!("{}.c", lorem::en::Word().fake::<String>());

    let mut cmd = Command::new(cargo::cargo_bin!());
    cmd.arg(&path);

    let error = format!("File {path} does not exist");
    cmd.assert().failure().stderr(contains(error));
}

#[test]
fn test_returns_error_if_path_does_not_have_c_extension() {
    let file: NamedTempFile = NamedTempFile::with_suffix(".mock").unwrap();

    let mut cmd = Command::new(cargo::cargo_bin!());
    cmd.arg(file.path());

    let error = format!("File must be a .c file: {}", file.path().display());
    cmd.assert().failure().stderr(contains(error));
}
