use std::error::Error;

use handlebars::Handlebars;
use serde_json::Value;
use quick_xml::reader::Reader;
use quick_xml::events::Event;

#[derive(Debug, Clone)]
pub struct ExistDb<'a> {
    renderer: Handlebars<'a>,
    uri: String,
}

fn make_renderer<'a>() -> Handlebars<'a> {
    let mut hbs = Handlebars::new();

    hbs.register_template_file("header", "./src/queries/header.hbs").unwrap();
    hbs.register_template_file("footer", "./src/queries/footer.hbs").unwrap();
    hbs.register_template_file("queryCount", "./src/queries/query_count.hbs").unwrap();
    hbs.register_template_file("queryRowCount", "./src/queries/query_row_count.hbs").unwrap();

    hbs
}

pub fn new_existdb<'a>(uri: String) -> ExistDb<'a> {
    ExistDb {
        renderer: make_renderer(),
        uri,
    }
}

impl ExistDb<'_> {
    pub async fn query_db(
        &self,
        user_id: &str,
        query_name: &str,
        data: Value,
    ) -> Result<String, Box<dyn Error>> {
        let query = self.renderer.render(query_name, &data)?;

        let client = reqwest::Client::new();
        let uri = format!("{}/{}", &self.uri, &user_id);
        println!("existdb uri: {}", &uri);
        let request = client.post(uri)
            .body(query);
        let response = request
            .send()
            .await?
            .text()
            .await?;

        let mut reader = Reader::from_str(&response);
        reader.config_mut().trim_text(true);

        let mut buf = Vec::new();
        println!("=============================== {}", &response);
        loop {
            match reader.read_event_into(&mut buf) {
                Ok(Event::Start(tag)) => println!("start {:#?}", String::from_utf8(Vec::from(tag.name().local_name().into_inner()))?),
                Ok(Event::Text(text)) => println!("text {}", text.unescape().unwrap().into_owned()),
                Ok(Event::End(tag)) => println!("end {:#?}", String::from_utf8(Vec::from(tag.local_name().into_inner()))?),
                Err(e) => panic!("Error at position {}: {:?}", reader.error_position(), e),
                Ok(Event::Eof) => break,
                _ => (),
            }
        }

        Ok(response)
    }
}

