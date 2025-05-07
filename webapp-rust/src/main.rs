// use std::error::Error;
use std::sync::Arc;
use axum::{
    routing::get,
    extract::{Path, State},
    Router,
};
use serde_json::json;
use handlebars::Handlebars;

#[derive(Debug)]
struct AppState {
    db: &'static str // TODO: implement
}

async fn get_foo() -> &'static str {
    "Hello foo!"
}

async fn post_foo() -> &'static str {
    "Hello foo post!"
}

fn make_renderer() -> Handlebars<'static> {
    let mut hbs = Handlebars::new();

    hbs.register_template_file("header", "./src/queries/header.hbs").unwrap();
    hbs.register_template_file("footer", "./src/queries/footer.hbs").unwrap();
    hbs.register_template_file("queryCount", "./src/queries/query_count.hbs").unwrap();
    hbs.register_template_file("queryRowCount", "./src/queries/query_row_count.hbs").unwrap();

    hbs
}

async fn query_row_count(
    Path((start, end)): Path<(i32, i32)>,
    State(state): State<Arc<AppState>>
) -> String {
    let hbs = make_renderer();

    println!("app_state: {:#?}\n", state);
    println!("db: {:#?}\n", state.db);

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
    hbs.render("queryRowCount", &data).unwrap()
}

async fn query_count(Path((start, end)): Path<(i32, i32)>) -> String {
    let hbs = make_renderer();

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
    let app_state = Arc::new(AppState {
        db: "thedb"
    });

    let app = Router::new()
        .route("/", get(|| async { "Hello world!" }))
        .route("/foo", get(get_foo).post(post_foo))
        .route("/query/rowcount/{start}/{end}", get(query_row_count))
        .route("/query/count/{start}/{end}", get(query_count))
        .with_state(app_state);

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

