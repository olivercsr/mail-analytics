// use std::error::Error;
use axum::{
    routing::get,
    extract::Path,
    Router,
};
use serde_json::json;
use handlebars::Handlebars;

async fn get_foo() -> &'static str {
    "Hello foo!"
}

async fn post_foo() -> &'static str {
    "Hello foo post!"
}

async fn query(Path((start, end)): Path<(i32, i32)>) -> String {
    let mut hbs = Handlebars::new();

    hbs.register_template_file("header", "./src/queries/header.hbs").unwrap();
    hbs.register_template_file("footer", "./src/queries/footer.hbs").unwrap();
    hbs.register_template_file("queryCount", "./src/queries/query_row_count.hbs").unwrap();

    let data = json!({
        "variables": [
            {
                "key": "start",
                "type": "integer",
                "value": start,
            },
            {
                "key": "end",
                "type": "integer",
                "value": end
            }
        ]
    });
    hbs.render("queryCount", &data).unwrap()
}

#[tokio::main]
async fn main() {
    let app = Router::new()
        .route("/", get(|| async { "Hello world!" }))
        .route("/foo", get(get_foo).post(post_foo))
        .route("/query/count/{start}/{end}", get(query));

    let listener = tokio::net::TcpListener::bind("0.0.0.0:8081").await.unwrap();
    axum::serve(listener, app).await.unwrap();
}

// #[macro_use] extern crate rocket;
//
// #[get("/")]
// fn index() -> &'static str {
//     "Hello world!"
// }
//
// #[launch]
// fn rocket() -> _ {
//     rocket::build().mount("/", routes![index])
// }

// fn main() {
//     println!("Hello, world!");
// }

