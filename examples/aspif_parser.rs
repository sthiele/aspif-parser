use std::fs::File;
use std::io::BufReader;
use std::path::Path;

fn main() {
    let path = Path::new("theory.aspif");

    let file = File::open(path).unwrap();
    let buf_reader = BufReader::new(file);
    match aspif::read_aspif(buf_reader) {
        Ok(result) => println!("{:?}", result),

        Err(e) => {
            let res = format!("Failed to parse aspif!\n{:?}", e);
            println!("{}", &res);
        }
    }
}
