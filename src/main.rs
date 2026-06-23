use clap::Parser;
use nora::compiler::Toolchain;

fn main() -> Result<(), String> {
    let driver = nora::driver::CompilerDriver::parse();

    if driver.path.extension().is_none_or(|e| e != "c") {
        let file_path = driver.path.display();
        return Err(format!("Input file must be a valid .c file: {file_path}"));
    }
    todo!()
}