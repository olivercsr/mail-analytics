use std::collections::HashMap;
use std::io::Read;
use xml::reader::{EventReader, XmlEvent};

pub fn read_xml<R: Read>(buf: R) {
    let parser = EventReader::new(buf);

    let mut tags: HashMap<String, usize> = HashMap::new();

    let mut depth = 0;
    for e in parser {
        match e {
            Ok(XmlEvent::StartElement { name, .. }) => {
                let tagname = name.to_string();
                let count = tags.get(&tagname);
                if count.is_some() {
                    tags.insert(String::from(&tagname), *count.unwrap()+1);
                } else {
                    tags.insert(String::from(&tagname), 1);
                }

                println!("{:spaces$}+{name} ({})", "", tags.get(&tagname).unwrap(), spaces = depth * 2);
                depth += 1;
            }
            Ok(XmlEvent::EndElement { name }) => {
                let tagname = name.to_string();
                //let count = tags.get(&tagname);
                //if count.is_some() {
                //    tags.insert(String::from(&tagname), *count.unwrap()+1);
                //} else {
                //    tags.insert(String::from(&tagname), 1);
                //}

                depth -= 1;
                println!("{:spaces$}-{name} ({})", "", tags.get(&tagname).unwrap(), spaces = depth * 2);
            }
            Err(e) => {
                eprintln!("Error: {e}");
                break;
            }
            _ => {}
        }
    }

    //Ok({});
}
