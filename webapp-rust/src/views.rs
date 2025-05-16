use std::error::Error;

use handlebars::Handlebars;
use serde_json::Value;

#[derive(Debug, Clone)]
pub struct Views<'a> {
    renderer: Handlebars<'a>
}

fn make_renderer<'a>() -> Handlebars<'a> {
    let mut hbs = Handlebars::new();

    hbs.register_template_file("pageHeader", "./src/views/pageHeader.hbs").unwrap();
    hbs.register_template_file("pageFooter", "./src/views/pageFooter.hbs").unwrap();
    hbs.register_template_file("navHeader", "./src/views/navHeader.hbs").unwrap();
    hbs.register_template_file("navFooter", "./src/views/navFooter.hbs").unwrap();
    hbs.register_template_file("queryResult", "./src/views/queryResult.hbs").unwrap();

    hbs
}

pub fn new_views<'a>() -> Views<'a> {
    Views {
        renderer: make_renderer(),
    }
}

impl Views<'_> {
    pub fn render_view(
        &self,
        view_name: &str,
        data: Value,
    ) -> Result<String, Box<dyn Error>> {
        Ok(self.renderer.render(view_name, &data)?)
    }
}
