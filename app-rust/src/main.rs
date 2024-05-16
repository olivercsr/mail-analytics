use std::fs::File;
use mail_analytics::{add, do_xml};

fn main() {
    println!("Hello, world: {}+{}={}!", 33, 44, add(33, 44));

    let file = File::open("./dmarc/google.com!csr-informatik.de!1707782400!1707868799.xml").unwrap();
    do_xml(file);
}
