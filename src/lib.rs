use std::fs::File;
use std::io::{BufReader/*, Result*/};
mod xmlreader;

pub fn do_xml(file: File) /*-> Result<()>*/ {
    //let file = File::open(path)?;
    let buffer = BufReader::new(file);

    xmlreader::read_xml(buffer);

    //Ok({})
}

pub fn add(left: usize, right: usize) -> usize {
    left + right
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn it_works() {
        let result = add(2, 2);
        assert_eq!(result, 4);
    }
}
