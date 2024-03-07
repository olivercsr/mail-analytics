use std::collections::HashMap;
use std::io::Read;
use chrono::{DateTime};
use xml::reader::{EventReader, XmlEvent};
use crate::dmarc::{Report};

pub fn read_xml<R: Read>(buf: R) {
    let parser = EventReader::new(buf);

    let mut tags: HashMap<String, usize> = HashMap::new();
    //let mut reports: HashMap<String, Report> = HashMap::new();
    let mut reports: Vec<Report> = Vec::new();

    let mut depth = 0;
    for e in parser {
        match e {
            Ok(XmlEvent::StartElement { name, attributes, .. }) => {
                let tagname = name.to_string();
                let count = tags.get(&tagname);
                if count.is_some() {
                    tags.insert(String::from(&tagname), *count.unwrap()+1);
                } else {
                    tags.insert(String::from(&tagname), 1);
                }

                if tagname.eq("record") {
                    let report = Report {
                        //reporter: "",
                        id: String::from(name.to_string()),
                        start: DateTime::from_timestamp(1000, 0).unwrap(),
                        end: DateTime::from_timestamp(2000, 0).unwrap(),
                        //policy: null()
                    };
                    reports.push(report);
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

    println!("reports: {:?}", reports);

    //Ok({});
}
