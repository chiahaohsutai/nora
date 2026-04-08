use std::path::Path;
use std::process::Command;

fn preprocess(input: &Path) -> Result<Command, String> {
    if !input.extension().is_some_and(|e| e.eq("c")) {
        return Err(String::from("Input file must have a .c extension"));
    }

    let mut output = input.to_path_buf();
    output.set_extension("i");

    let mut cmd = Command::new("gcc");
    cmd.arg("-E").arg("-P").arg(input).arg("-o").arg(output);

    Ok(cmd)
}

fn compile() {
    todo!()
}

fn assemble() {
    todo!()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_preprocess_returns_correct_gcc_cmd_and_args() {
        let input = Path::new("/foo.c");

        let cmd = preprocess(input).unwrap();
        let args: Vec<&str> = cmd.get_args().map(|arg| arg.to_str().unwrap()).collect();

        assert_eq!(vec!["-E", "-P", "/foo.c", "-o", "/foo.i"], args);
        assert_eq!(Some("gcc"), cmd.get_program().to_str());
    }

    #[test]
    fn test_preprocess_returns_error_on_incorrect_file_extension() {
        let input = Path::new("/foo.txt");

        let cmd = preprocess(input);
        let error = cmd.err().unwrap();

        assert_eq!(error, String::from("Input file must have a .c extension"));
    }
}
