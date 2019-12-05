use std::path::Path;

fn main() {
    let file = Path::new("theory.aspif");
    let buf = std::fs::read_to_string(file).unwrap();
    match aspif_parser::aspif_program(&buf) {
        Ok((_, result)) => println!("{:?}", result),
        Err(e) => println!("Failed to parse header: {:?}", e),
    }
}
