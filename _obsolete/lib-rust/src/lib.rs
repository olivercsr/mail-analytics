use std::fs::File;
use std::io::{BufReader/*, Result*/};
use std::path::Path;

mod xmlreader;
mod dmarc;

pub fn do_xml<R: AsRef<Path>>(path: R) /*-> Result<()>*/ {
    //let file = File::open(path)?;
    //let buffer = BufReader::new(file);

    xmlreader::read_xml(path);

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
