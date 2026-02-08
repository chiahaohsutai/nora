# NORA

NORA is a C compiler implemented in Rust, based on the curriculum from Nora Sandler’s [Writing a C Compiler: Build a Real Programming Language from Scratch](https://nostarch.com/writing-c-compiler).

This project presents a full implementation of the compiler described in the text. It supports a subset of the C language, including arithmetic expressions, bitwise operations, and control-flow statements. The compiler is structured into five compilation stages:

1. **Lexing (Tokenization)** – Converts the input source code into a stream of tokens.
2. **Parsing** – Builds an abstract syntax tree (AST) using recursive descent with precedence climbing.
3. **Semantic Aanlysis** – Validates that the AST is semantically correct (variable resolution, loop labeling, type checking).
4. **Tacky (Three-Address Code)** – Linearizes the AST into a sequence of simple instructions using three-address code (TAC).
5. **Code Generation** – Translates TAC instructions into target assembly code.
6. **Linking** – Assembles and links the generated assembly into a native executable.

## Command-Line Interface (CLI)

You can interact with the compiler through a command-line interface (CLI) to compile C programs into binaries or stop execution at specific stages. Note that the compiler targets the x86_64 architecture; therefore, your shell instance must be running on an x86_64 host to successfully compile and execute the generated binaries.

```bash
# Display available CLI options and flags
cargo run -- --help

# Compile the C program normally (runs all compilation stages)
cargo run -- path/to/my_program.c

# Run the compiler and stop after the lexing stage
cargo run -- --lex path/to/my_program.c

# Run the compiler and stop after the parsing stage (AST generation)
cargo run -- --parse path/to/my_program.c

# Run the compiler and stop after the validation stage (AST semantic validation)
cargo run -- --validate path/to/my_program.c

# Run the compiler and stop after the tacky stage (TAC generation)
cargo run -- --tacky path/to/my_program.c

# Run the compiler and stop after code generation (emit assembly)
cargo run -- --codegen path/to/my_program.c
```

## Local Installation

**Prerequisites**:

To build and run this project, ensure the following command-line tools are installed:

* The Rust toolchain, installed via `rustup`, with `cargo`
* A system C compiler (`gcc` or `clang`, accessible as `gcc`)

```bash
# 1. Clone the repo and its submodules
git clone --recurse-submodules https://github.com/path/to/repository
cd repository-name

# 2. Build the project
cargo build

# 3. (Apple Silicon Mac ONLY) If you hit architecture errors, switch to Intel mode:
arch -x86_64 zsh

# 4. Run the compiler on your source file
cargo run -- path/to/my_program.c
```

## Testing

This project uses the [writing-a-c-compiler-tests](https://github.com/nlsandler/writing-a-c-compiler-tests/) test suite. Please clone the repository and follow the repository's instructions to set up and run the tests.

```bash
# Run the test suite against the compiled binary
cd writing-a-c-compiler-tests
./test_compiler ../path/to/project/binary --extra-credit
```
