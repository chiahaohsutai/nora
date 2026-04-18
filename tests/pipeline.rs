use std::env::temp_dir;
use std::fs::{remove_file, write};
use std::path::PathBuf;

use predicates::Predicate;
use predicates::str::contains;
use rand::distr::{Alphanumeric, SampleString};
use rand::rng;

use nora::{Gcc, Linker, Preprocessor};

fn random_temp_file_path<T: AsRef<str>>(ext: Option<T>, len: usize) -> PathBuf {
    let stem = Alphanumeric.sample_string(&mut rng(), len);
    let name = match ext {
        Some(ext) => format!("{stem}.{}", ext.as_ref()),
        None => stem,
    };
    temp_dir().join(PathBuf::from(name))
}

#[test]
fn gcc_preprocessor_fails_if_command_does_not_exist() {
    let cmd = Alphanumeric.sample_string(&mut rng(), 10);
    let input = random_temp_file_path(Some("c"), 24);
    let output = random_temp_file_path(Some("i"), 24);

    let gcc = Gcc::new(cmd);
    let res = gcc.preprocess(&input, &output);

    assert!(res.is_err());
    assert!(res.is_err_and(|e| contains("Failed to invoke command").eval(&e)))
}

#[test]
fn gcc_preprocessor_fails_if_file_does_not_exist() {
    let input = random_temp_file_path(Some("c"), 24);
    let output = random_temp_file_path(Some("i"), 24);

    let gcc = Gcc::default();
    let res = gcc.preprocess(&input, &output);

    assert!(res.is_err());
    assert!(res.is_err_and(|e| contains("Command failed with error").eval(&e)));
}

#[test]
fn gcc_preprocessor_fails_if_input_file_does_not_have_c_extension() {
    let input = random_temp_file_path(Some("mock"), 24);
    let output = random_temp_file_path(Some("i"), 24);

    let gcc = Gcc::default();
    let res = gcc.preprocess(&input, &output);

    assert!(res.is_err());
    assert!(res.is_err_and(|e| contains("Input path must have a .c extension").eval(&e)));
}

#[test]
fn gcc_preprocessor_fails_if_output_path_does_not_have_i_extension() {
    let input = random_temp_file_path(Some("c"), 24);
    let output = random_temp_file_path(Some("mock"), 24);

    let gcc = Gcc::default();
    let res = gcc.preprocess(&input, &output);

    assert!(res.is_err());
    assert!(res.is_err_and(|e| contains("Output path must have a .i extension").eval(&e)));
}

#[test]
fn gcc_preprocessor_generates_intermediate_file() {
    let input = random_temp_file_path(Some("c"), 24);
    let output = random_temp_file_path(Some("i"), 24);
    write(&input, "int main(){return 0;}").unwrap();

    let gcc = Gcc::default();
    let res = gcc.preprocess(&input, &output);
    let res = res.as_ref();

    assert!(res.is_ok_and(|path| path.exists()));
    assert!(res.is_ok_and(|path| path.extension().is_some_and(|ext| ext.eq("i"))));
    remove_file(&input).unwrap();
    remove_file(&output).unwrap();
}

#[test]
fn gcc_linker_fails_if_command_does_not_exist() {
    let cmd = Alphanumeric.sample_string(&mut rng(), 10);
    let input = random_temp_file_path(Some("s"), 24);
    let output = random_temp_file_path::<String>(None, 24);

    let gcc = Gcc::new(cmd);
    let res = gcc.link(&input, &output);

    assert!(res.is_err());
    assert!(res.is_err_and(|e| contains("Failed to invoke command").eval(&e)))
}

#[test]
fn gcc_linker_fails_if_file_does_not_exist() {
    let input = random_temp_file_path(Some("s"), 24);
    let output = random_temp_file_path::<String>(None, 24);

    let gcc = Gcc::default();
    let res = gcc.link(&input, &output);

    assert!(res.is_err());
    assert!(res.is_err_and(|e| contains("Command failed with error").eval(&e)));
}

#[test]
fn gcc_linker_fails_if_input_file_does_not_have_s_or_o_extension() {
    let input = random_temp_file_path(Some("mock"), 24);
    let output = random_temp_file_path::<String>(None, 24);

    let gcc = Gcc::default();
    let res = gcc.link(&input, &output);

    assert!(res.is_err());
    assert!(res.is_err_and(|e| contains("Input path must have a .s or .o extension").eval(&e)));
}

#[test]
fn gcc_linker_fails_if_output_path_has_an_extension() {
    let input = random_temp_file_path(Some("s"), 24);
    let output = random_temp_file_path(Some("mock"), 24);

    let gcc = Gcc::default();
    let res = gcc.link(&input, &output);

    assert!(res.is_err());
    assert!(res.is_err_and(|e| contains("Output path should not have an extension").eval(&e)));
}

#[test]
fn gcc_linker_generates_executable_file() {
    let input = random_temp_file_path(Some("s"), 24);
    let output = random_temp_file_path::<String>(None, 24);
    write(&input, ".globl _main\n_main:\nxorl %eax, %eax\nretq").unwrap();

    let gcc = Gcc::default();
    let res = gcc.link(&input, &output);
    let res = res.as_ref();

    assert!(res.is_ok_and(|path| path.exists()));
    assert!(res.is_ok_and(|path| path.extension().is_none()));
    remove_file(&input).unwrap();
    remove_file(&output).unwrap();
}
