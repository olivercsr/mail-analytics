use handlebars::Handlebars;
use serde_json::Value;

#[derive(Debug, Clone)]
pub struct ExistDb<'a> {
    renderer: Handlebars<'a>,
    uri: String,
}

fn make_renderer() -> Handlebars<'static> {
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
        query_name: &str,
        data: Value,
    ) -> String {
        self.renderer.render(query_name, &data).unwrap()
    }
}

