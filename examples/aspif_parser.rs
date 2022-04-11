use std::path::Path;

fn main() {
    let file = Path::new("theory.aspif");
    let buf = std::fs::read_to_string(file).unwrap();
    match aspif::read_aspif(&buf) {
        Ok(result) => println!("{:?}", result),

        Err(e) => {
            let res = format!("Failed to parse aspif!\n{:?}", e);
            println!("{}", &res);
        }
    }
}
