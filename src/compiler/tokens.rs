pub enum Token {}

pub trait Tokenizer {
    fn tokenize<T>(input: T) -> Result<Vec<Token>, String>
    where
        T: Iterator<Item = Result<char, String>>;
}
