use std::env::temp_dir;
use std::fs::{remove_file, write};
use std::path::PathBuf;

use predicates::Predicate;
use predicates::str::contains;
use rand::distr::{Alphanumeric, SampleString};
use rand::rng;

use nora::{GccPreprocessor, Preprocessor};

fn random_temp_file_path<T: AsRef<str>>(ext: T, len: usize) -> PathBuf {
    let stem = Alphanumeric.sample_string(&mut rng(), len);
    temp_dir().join(PathBuf::from(format!("{stem}.{}", ext.as_ref())))
}

#[test]
fn gcc_preprocessor_fails_if_command_does_not_exist() {
    let cmd = Alphanumeric.sample_string(&mut rng(), 10);
    let input = random_temp_file_path(".c", 24);
    let output = random_temp_file_path(".i", 24);

    let pre = GccPreprocessor::new(cmd);
    let res = pre.preprocess(&input, &output);
    assert!(res.is_err_and(|e| contains("Failed to execute command").eval(&e)))
}

#[test]
fn gcc_preprocessor_fails_if_file_does_not_exist() {
    let input = random_temp_file_path(".c", 24);
    let output = random_temp_file_path(".i", 24);

    let pre = GccPreprocessor::default();
    let res = pre.preprocess(&input, &output);
    assert!(res.is_err_and(|e| contains("Preprocessing failed").eval(&e)));
}

#[test]
fn gcc_preprocessor_generates_intermediate_file() {
    let input = random_temp_file_path(".c", 24);
    let output = random_temp_file_path(".i", 24);
    write(&input, "int main(){return 0;}").unwrap();

    let pre = GccPreprocessor::default();
    let res = pre.preprocess(&input, &output);
    let res = res.as_ref();

    assert!(res.is_ok_and(|path| path.exists()));
    assert!(res.is_ok_and(|path| path.extension().is_some_and(|ext| ext.eq("i"))));
    remove_file(&input).unwrap();
    remove_file(&output).unwrap();
}
