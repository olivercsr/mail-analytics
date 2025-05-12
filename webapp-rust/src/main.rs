// use std::error::Error;
// use std::sync::Arc;
use clap::Parser;
use axum::{
    extract::{Request, Path, State, Extension},
    http::header::HeaderMap,
    http::StatusCode,
    middleware::Next,
    response::Response,
    Router,
    routing::get,
};
use tower::ServiceBuilder;
use serde_json::json;
use handlebars::{
    Handlebars,
    //registry::Registry,
};

#[derive(Parser, Debug, Clone)]
struct Cli {
    #[arg(short, long, default_value = "remote-user")]
    authuser_header: String,
    #[arg(long)]
    dev_authuser: Option<String>,
    #[arg(short, long)]
    existdb_uri: String,
}

#[derive(Debug, Clone)]
struct ExistDb {
    uri: String,
}

#[derive(Debug, Clone)]
struct AppState<'a> {
    cli: Cli,
    query_renderer: Handlebars<'a>,
    db: ExistDb
}

#[derive(Debug, Clone)]
struct UserId {
    user_id: String
}

async fn auth_header<'a>(
    State(state): State<AppState<'a>>,
    mut req: Request,
    next: Next
) -> Result<Response, StatusCode> {
    let userid_opt = req.headers()
        .get(state.cli.authuser_header)
        .and_then(|header| Some(String::from(header.to_str().unwrap())));

    let userid = if let Some(userid) = userid_opt {
        userid
    } else {
        return Err(StatusCode::UNAUTHORIZED);
    };

    req.extensions_mut().insert(UserId { user_id: userid });

    Ok(next.run(req).await)
}

async fn get_foo() -> &'static str {
    "Hello foo!"
}

async fn post_foo() -> &'static str {
    "Hello foo post!"
}

async fn query_row_count<'a>(
    headers: HeaderMap,
    Path((start, end)): Path<(u32, u32)>,
    State(state): State<AppState<'a>>,
    Extension(userid): Extension<UserId>,
) -> String {
    println!("headers: {:#?}\n", headers);
    println!("app_state: {:#?}\n", state);
    println!("db: {:#?}\n", state.db);
    println!("userid: {:#?} {:#?}\n", userid, userid.user_id);

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
    state.query_renderer.render("queryRowCount", &data).unwrap()
}

async fn query_count<'a>(
    Path((start, end)): Path<(i32, i32)>,
    State(state): State<AppState<'a>>,
) -> String {
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
    state.query_renderer.render("queryCount", &data).unwrap()
}

fn make_renderer() -> Handlebars<'static> {
    let mut hbs = Handlebars::new();

    hbs.register_template_file("header", "./src/queries/header.hbs").unwrap();
    hbs.register_template_file("footer", "./src/queries/footer.hbs").unwrap();
    hbs.register_template_file("queryCount", "./src/queries/query_count.hbs").unwrap();
    hbs.register_template_file("queryRowCount", "./src/queries/query_row_count.hbs").unwrap();

    hbs
}

#[tokio::main]
async fn main() {
    let args = Cli::parse();

    let app_state = AppState {
        db: ExistDb { uri: String::from(&args.existdb_uri) },
        cli: args,
        query_renderer: make_renderer(),
    };

    let app = Router::new()
        .route("/", get(|| async { "Hello world!" }))
        .route("/foo", get(get_foo).post(post_foo))
        .route("/query/rowcount/{start}/{end}", get(query_row_count))
        .route("/query/count/{start}/{end}", get(query_count))
        .layer(
            ServiceBuilder::new()
                .layer(axum::middleware::from_fn_with_state(app_state.clone(), auth_header)),
        )
        // .layer(axum::middleware::from_fn(auth_header))
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

