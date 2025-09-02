use std::str;
use std::collections::HashMap;
use std::path::Path;
use std::io::BufReader;
use std::fs::File;
use chrono::DateTime;
use quick_xml::reader::Reader;
use quick_xml::events::Event;
use crate::dmarc::{Report, ReportMetadata};

fn read_report_metadata(reader: &mut Reader<BufReader<File>>) {
    let mut buf = Vec::new();

    loop {
        match reader.read_event_into(&mut buf) {
            Ok(Event::Start(_)) => {
            },
            Ok(Event::End(_)) => {
            },
            Err(err) => {
                eprintln!("Error: {err}");
                break;
            },
            Ok(Event::Eof) => break,
            _ => {}
        }

        buf.clear();
    }
}

pub fn read_xml<R: AsRef<Path>>(path: R) {
    let mut reader = Reader::from_file(path).unwrap();
    reader.trim_text(true);

    let mut tags: HashMap<String, usize> = HashMap::new();
    //let mut reports: HashMap<String, Report> = HashMap::new();
    let mut reports: Vec<Report> = Vec::new();

    let mut buf = Vec::new();
    let mut depth = 0;
    loop {
        match reader.read_event_into(&mut buf) {
            Ok(Event::Start(e)) => {
                let local_name = e.local_name();
                let tagname = str::from_utf8(local_name.as_ref()).unwrap();
                let count = tags.get(tagname);
                if count.is_some() {
                    tags.insert(String::from(tagname), *count.unwrap()+1);
                } else {
                    tags.insert(String::from(tagname), 1);
                }

                if tagname.eq("report_metadata") {
                    //reports.push(report);
                    let report_metadata = read_report_metadata(&mut reader);
                }
                

                println!("{:spaces$}+{tagname} ({})", "", tags.get(tagname).unwrap(), spaces = depth * 2);
                depth += 1;
            },
            Ok(Event::Text(t)) => {
                let tagname = str::from_utf8(t.as_ref()).unwrap();
                //let count = tags.get(&tagname);
                //if count.is_some() {
                //    tags.insert(String::from(&tagname), *count.unwrap()+1);
                //} else {
                //    tags.insert(String::from(&tagname), 1);
                //}

                depth -= 1;
                println!("{:spaces$}-{tagname} ({})", "", tags.get(tagname).unwrap(), spaces = depth * 2);
            },
            Ok(Event::Eof) => break,
            Err(err) => {
                eprintln!("Error: {err}");
                break;
            }
            _ => {}
        }
        buf.clear();
    }

    println!("reports: {:?}", reports);

    //Ok({});
}
