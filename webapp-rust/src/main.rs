use axum::{
    routing::get,
    Router,
};

async fn get_foo() -> &'static str {
    "Hello foo!"
}

async fn post_foo() -> &'static str {
    "Hello foo post!"
}

#[tokio::main]
async fn main() {
    let app = Router::new()
        .route("/", get(|| async { "Hello world!" }))
        .route("/foo", get(get_foo).post(post_foo));

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

