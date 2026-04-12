use std::env::temp_dir;
use std::fs::{remove_file, write};
use std::path::Path;

use predicates::Predicate;
use predicates::str::contains;
use rand::distr::{Alphanumeric, SampleString};
use rand::rng;

use nora::{GccPreprocessor, Preprocessor};

#[test]
fn gcc_preprocessor_fails_if_command_does_not_exist() {
    let cmd = Alphanumeric.sample_string(&mut rng(), 24);
    let filename = format!("{}.c", Alphanumeric.sample_string(&mut rng(), 24));

    let path = Path::new(&filename);
    let pre = GccPreprocessor::new(cmd);
    let result = pre.preprocess(&path);

    assert!(result.is_err_and(|e| contains("Failed to execute command").eval(&e)))
}

#[test]
fn gcc_preprocessor_fails_if_file_does_not_exist() {
    let filename = format!("{}.c", Alphanumeric.sample_string(&mut rng(), 24));
    let pre = GccPreprocessor::default();

    let path = Path::new(&filename);
    let result = pre.preprocess(path);

    assert!(result.is_err_and(|e| contains("Preprocessing failed").eval(&e)));
}

#[test]
fn gcc_preprocessor_generates_intermediate_file() {
    let filestem = Alphanumeric.sample_string(&mut rng(), 24);
    let path = temp_dir().join(format!("{filestem}.c"));

    write(&path, "int main(){return 0;}").unwrap();

    let pre = GccPreprocessor::default();
    let result = pre.preprocess(&path);

    assert!(result.is_ok());
    let output = result.unwrap();

    assert!(output.exists());
    assert!(output.extension().is_some_and(|ext| ext.eq("i")));

    remove_file(&path).unwrap();
}
