use std::path::Path;
use std::process::Command;

fn preprocess(input_file: &Path) -> Result<Command, String> {
    if !input_file.is_file() || input_file.extension().unwrap() != "c" {
        return Err("Input path must be a file with .c extension".into());
    }
    let file_name = input_file.file_stem().unwrap();
    let file_dir = input_file.parent().unwrap();

    let mut output = file_dir.join(file_name);
    output.set_extension("i");

    let output_path = output.as_os_str();
    let input_path = input_file.as_os_str();

    let mut gcc_cmd = Command::new("gcc");
    gcc_cmd
        .arg("-E")
        .arg("-P")
        .arg(input_path)
        .arg("-o")
        .arg(output_path);

    Ok(gcc_cmd)
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
    use std::ffi::OsStr;

    use tempfile::NamedTempFile;

    #[test]
    fn test_preprocess_returns_valid_gcc_cmd() {
        let temp = NamedTempFile::with_suffix(".c").unwrap();
        let input_file = temp.path();

        let cmd = preprocess(input_file);
        let args: Result<Vec<String>, _> = cmd.as_ref().map(|cmd| {
            cmd.get_args()
                .map(|arg| arg.to_str().unwrap())
                .map(|arg| String::from(arg))
                .collect()
        });
        let file_name = input_file.file_stem().unwrap();

        let mut output_path = input_file.parent().unwrap().join(file_name);
        output_path.set_extension("i");

        let input_path = input_file.to_str().unwrap();
        let output_path = output_path.to_str().unwrap();

        let expected = vec!["-E", "-P", input_path, "-o", output_path];
        assert_eq!(expected, args.unwrap());
        assert_eq!(Some("gcc"), cmd.unwrap().get_program().to_str());
    }

    #[test]
    fn test_preprocess_non_file_input() {
        let input_file = Path::new("/mock_directory/");
        let cmd = preprocess(input_file);
        assert!(cmd.is_err())
    }

    #[test]
    fn test_preprocess_nonexistent_file() {
        let input_file = Path::new("/foo.c");
        let cmd = preprocess(input_file);
        assert!(cmd.is_err())
    }

    #[test]
    fn test_preprocess_incorrect_extension() {
        let input_file = Path::new("/foo.txt");
        let cmd = preprocess(input_file);
        assert!(cmd.is_err())
    }
}
