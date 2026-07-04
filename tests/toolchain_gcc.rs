use std::error::Error;
use std::fs::{read_to_string, write};
use std::path::PathBuf;

use assert_fs::TempDir;
use nanoid::nanoid;

use nora::compiler::{GccToolchain, Toolchain};

#[test]
fn preprocess_returns_error_on_nonexistent_file() -> Result<(), Box<dyn Error>> {
    let tempdir = TempDir::new()?;
    let input = PathBuf::from(format!("{}.c", nanoid!()));
    let output = tempdir.path().join(format!("{}.i", nanoid!()));

    let toolchain = GccToolchain::default();
    let result = toolchain.preprocess(&input, &output);

    assert!(result.is_err_and(|e| e.contains("Invalid C program file")));
    Ok(())
}

#[test]
fn preprocess_returns_error_on_non_c_file() -> Result<(), Box<dyn Error>> {
    let tempdir = TempDir::new()?;
    let input = PathBuf::from(format!("{}.c", nanoid!()));
    let output = tempdir.path().join(format!("{}.i", nanoid!()));

    let toolchain = GccToolchain::default();
    let result = toolchain.preprocess(&input, &output);

    assert!(result.is_err_and(|e| e.contains("Invalid C program file")));
    Ok(())
}

#[test]
fn preprocess_returns_error_on_directory_input() -> Result<(), Box<dyn Error>> {
    let tempdir = TempDir::new()?;
    let output = tempdir.path().join(format!("{}.i", nanoid!()));

    let toolchain = GccToolchain::default();
    let result = toolchain.preprocess(&tempdir, &output);

    assert!(result.is_err_and(|e| e.contains("Invalid C program file")));
    Ok(())
}

#[test]
fn preprocess_creates_output_from_valid_source() -> Result<(), Box<dyn Error>> {
    let tempdir = TempDir::new()?;
    let input = tempdir.path().join(format!("{}.c", nanoid!()));
    let output = tempdir.path().join(format!("{}.i", nanoid!()));

    let program = "int main() { return 0; }";
    write(&input, program)?;

    let toolchain = GccToolchain::default();
    let result = toolchain.preprocess(&input, &output);
    assert!(result.is_ok());

    let content = read_to_string(&output)?;
    assert!(content.contains(program));
    Ok(())
}

#[test]
fn preprocess_returns_error_when_program_not_found() -> Result<(), Box<dyn Error>> {
    let tempdir = TempDir::new()?;
    let input = tempdir.path().join(format!("{}.c", nanoid!()));
    let output = tempdir.path().join(format!("{}.i", nanoid!()));

    let program = "int main() { return 0; }";
    write(&input, program)?;

    let toolchain = GccToolchain::new(&nanoid!());
    let result = toolchain.preprocess(&input, &output);

    assert!(result.is_err_and(|e| e.contains("Preprocessing command failed")));
    Ok(())
}

#[test]
fn assemble_and_link_returns_error_on_nonexistent_file() -> Result<(), Box<dyn Error>> {
    let input = PathBuf::from(format!("{}.s", nanoid!()));
    let output = PathBuf::from(format!("{}", nanoid!()));

    let toolchain = GccToolchain::default();
    let result = toolchain.assemble_and_link(&input, &output);

    assert!(result.is_err_and(|e| e.contains("Invalid assembly file")));
    Ok(())
}

#[test]
fn assemble_and_link_returns_error_on_non_assembly_file() -> Result<(), Box<dyn Error>> {
    let input = PathBuf::from(format!("{}.mock", nanoid!()));
    let output = PathBuf::from(format!("{}", nanoid!()));

    let toolchain = GccToolchain::default();
    let result = toolchain.assemble_and_link(&input, &output);

    assert!(result.is_err_and(|e| e.contains("Invalid assembly file")));
    Ok(())
}

#[test]
fn assemble_and_link_returns_error_on_directory_input() -> Result<(), Box<dyn Error>> {
    let tempdir = TempDir::new()?;
    let output = PathBuf::from(format!("{}", nanoid!()));

    let toolchain = GccToolchain::default();
    let result = toolchain.assemble_and_link(tempdir.path(), &output);

    assert!(result.is_err_and(|e| e.contains("Invalid assembly file")));
    Ok(())
}

#[test]
fn assemble_and_link_produces_binary_from_valid_assembly() -> Result<(), Box<dyn Error>> {
    let tempdir = TempDir::new()?;
    let input = tempdir.path().join(format!("{}.s", nanoid!()));
    let output = tempdir.path().join(nanoid!());

    let program = ".globl _main\n_main:\n\txorl %eax, %eax\n\tret\n";
    write(&input, program)?;

    let toolchain = GccToolchain::default();
    let result = toolchain.assemble_and_link(&input, &output);

    println!("{:?}", result);
    assert!(result.is_ok_and(|_| output.is_file()));
    Ok(())
}

#[test]
fn assemble_and_link_returns_error_when_gcc_not_found() -> Result<(), Box<dyn Error>> {
    let tempdir = TempDir::new()?;
    let input = tempdir.path().join(format!("{}.s", nanoid!()));
    let output = tempdir.path().join( nanoid!());

    let program = ".globl _main\n_main:\n\txorl %eax, %eax\n\tret";
    write(&input, program)?;

    let toolchain = GccToolchain::new(&nanoid!());
    let result = toolchain.assemble_and_link(&input, &output);

    assert!(result.is_err_and(|e| e.contains("Linking command failed")));
    Ok(())
}
