use std::ffi::OsStr;

use clap::Parser;

use nora::{Cli, Phase, compile};

fn main() -> Result<(), String> {
    let cli = Cli::parse();

    if !cli.path.exists() {
        return Err(format!("File {} does not exist", cli.path.display()));
    }
    if cli.path.extension().unwrap_or(OsStr::new("")) != "c" {
        return Err(format!("File must be a .c file: {}", cli.path.display()));
    }

    let stop_after = Option::<Phase>::from(cli.stage);
    Ok(())
}
