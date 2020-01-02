use nom::error::{VerboseError, VerboseErrorKind};
use nom::Err;
use std::path::Path;

//TODO: remove once nom issue #986 is fixed
/// transforms a `VerboseError` into a trace with input position information
fn convert_error(input: &str, e: VerboseError<&str>) -> nom::lib::std::string::String {
    use nom::{lib::std::fmt::Write, Offset};

    let mut result = nom::lib::std::string::String::new();

    for (i, (substring, kind)) in e.errors.iter().enumerate() {
        let offset = input.offset(substring);

        let line_begin = memchr::memrchr(b'\n', input[..offset].as_bytes())
            // Don't include preceding newline
            .map(|off| off + 1)
            .unwrap_or(0);
        let line_end = memchr::memchr(b'\n', input[offset..].as_bytes())
            .map(|off| off + offset)
            .unwrap_or(input.len());
        let line = &input[line_begin..line_end];

        let line_num = input[..line_begin]
            .bytes()
            .fold(0, |acc, b| acc + (b == b'\n') as usize);
        let column = offset - line_begin;

        match kind {
            VerboseErrorKind::Char(c) => {
                writeln!(result, "{}: at line {}:", i, line_num).unwrap();
                result += &line;
                result += "\n";

                // Format right aligned with width one greater than column (width includes the ^ char)
                writeln!(result, "{1:>0$}", column + 1, '^').unwrap();
                // Debug fmt, for chars, in case of control chars
                write!(result, "expected {:?}, found ", c).unwrap();
                match substring.chars().next() {
                    Some(next_char) => write!(result, "{:?}", next_char).unwrap(),
                    None => write!(result, "EOF").unwrap(),
                }
                result += "\n\n";
            }
            VerboseErrorKind::Context(s) => {
                writeln!(result, "{}: at line {}, in {}:", i, line_num, s).unwrap();
                result += &line;
                result += "\n";
                // Format right aligned with width one greater than column (width includes the ^ char)
                writeln!(result, "{1:>0$}\n", column + 1, '^').unwrap();
            }
            VerboseErrorKind::Nom(e) => {
                writeln!(result, "{}: at line {}, in {:?}:", i, line_num, e).unwrap();
                result += &line;
                result += "\n";
                // Format right aligned with width one greater than column (width includes the ^ char)
                writeln!(result, "{1:>0$}\n", column + 1, '^').unwrap();
            }
        }
    }

    result
}

fn main() {
    let file = Path::new("theory.aspif");
    let buf = std::fs::read_to_string(file).unwrap();
    match aspif_parser::aspif_program::<VerboseError<&str>>(&buf) {
        Ok((_, result)) => println!("{:?}", result),
        Err(Err::Error(e)) | Err(Err::Failure(e)) => {
            println!("Failed to parse aspif!\n{}", convert_error(&buf, e))
        }
        Err(e) => println!("Failed to parse aspif: {:?}", e),
    }
}
