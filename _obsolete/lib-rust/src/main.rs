use std::fs::File;
use mail_analytics::{add, do_xml};

#[tokio::main]
async fn main() {
    let filepath = String::from("./dmarc/google.com!csr-informatik.de!1707782400!1707868799.xml");

    println!("Hello, world: {}+{}={}!", 33, 44, add(33, 44));

    //let file = File::open().unwrap();
    do_xml(filepath);
}
