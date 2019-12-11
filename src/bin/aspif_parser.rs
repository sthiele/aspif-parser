use nom::error::{convert_error, VerboseError};
use nom::Err;
use std::path::Path;
fn main() {
    let file = Path::new("theory.aspif");
    let buf = std::fs::read_to_string(file).unwrap();
    match aspif_parser::aspif_program::<VerboseError<&str>>(&buf) {
        Ok((_, result)) => println!("{:?}", result),
        Err(Err::Error(e)) | Err(Err::Failure(e)) => {
            println!("Failed to parse aspif: {}", convert_error(&buf, e))
        }
        Err(e) => println!("Failed to parse aspif: {:?}", e),
    }
}
