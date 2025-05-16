use std::error::Error;

// use std::error::Error;
// use std::sync::Arc;
// use clap::Parser;
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

use crate::appconfig::{
    AppConfig,
    read_appconfig,
};
use crate::existdb::{
    ExistDb,
    new_existdb,
};
use crate::views::{
    Views,
    new_views,
};

mod appconfig;
mod existdb;
mod views;

// #[derive(Parser, Debug, Clone)]
// struct Cli {
//     #[arg(long, default_value = "0.0.0.0")]
//     host: String,
//     #[arg(short, long, default_value = "8080")]
//     port: u16,
//     #[arg(short, long, default_value = "remote-user")]
//     authuser_header: String,
//     #[arg(long)]
//     dev_authuser: Option<String>,
//     #[arg(short, long)]
//     existdb_uri: String,
// }

#[derive(Debug, Clone)]
struct AppState<'a> {
    // cli: Cli,
    appconfig: AppConfig,
    db: ExistDb<'a>,
    views: Views<'a>,
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
        .get(state.appconfig.auth_userheader)
        .and_then(|header| Some(String::from(header.to_str().unwrap())));

    let userid = if let Some(userid) = userid_opt {
        userid
    } else {
        return Err(StatusCode::UNAUTHORIZED);
    };

    req.extensions_mut().insert(UserId { user_id: userid });

    Ok(next.run(req).await)
}

async fn get_foo<'a>(
    State(state): State<AppState<'a>>,
) -> Result<String, StatusCode> {
    let data = json!({
        "title": "Hello foo!"
    });

    state.views.render_view("queryResult", data).map_err(|_| StatusCode::INTERNAL_SERVER_ERROR)
}

async fn post_foo() -> String {
    String::from("Hello foo post!")
}

async fn query_row_count<'a>(
    headers: HeaderMap,
    Path((start, end)): Path<(u32, u32)>,
    State(state): State<AppState<'a>>,
    Extension(userid): Extension<UserId>,
) -> Result<String, StatusCode> {
    // println!("headers: {:#?}\n", headers);
    // println!("app_state: {:#?}\n", state);
    // println!("db: {:#?}\n", state.db);
    // println!("userid: {:#?} {:#?}\n", userid, userid.user_id);

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

    let query_result = state.db.query_db(
        &userid.user_id,
        "queryRowCount",
        data
    ).await.map_err(|e| { println!("aaaaaaaa {:#?}", e); StatusCode::INTERNAL_SERVER_ERROR })?;

    state.views.render_view(
        "queryResult",
        query_result
    ).map_err(|e| {
            println!("bbbbbbbbbbbbbbbb {:#?}", e);
            StatusCode::INTERNAL_SERVER_ERROR
        })
}

async fn query_count<'a>(
    Path((start, end)): Path<(i32, i32)>,
    State(state): State<AppState<'a>>,
    Extension(userid): Extension<UserId>,
) -> Result<String, StatusCode> {
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

    let query_result = state.db.query_db(
        &userid.user_id,
        "queryCount",
        data
    ).await.map_err(|_| StatusCode::INTERNAL_SERVER_ERROR)?;

    state.views.render_view(
        "queryCount",
        query_result,
    ).map_err(|_| StatusCode::INTERNAL_SERVER_ERROR)
}

#[tokio::main]
async fn main() {
    // let args = Cli::parse();
    let appconfig = read_appconfig().unwrap();

    let app_state = AppState {
        // cli: args.clone(),
        appconfig: appconfig.clone(),
        db: new_existdb(String::from(&appconfig.existdb_uri)),
        views: new_views(),
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

    let bind_address = format!("{}:{}", &appconfig.host, &appconfig.port);
    let listener = tokio::net::TcpListener::bind(bind_address).await.unwrap();
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

