// Idiomatic actix-web — the extreme-speed case. HttpServer defaults its worker count
// to the number of logical CPUs, so this is multicore out of the box.
use actix_web::{web, App, HttpResponse, HttpServer};

async fn plaintext() -> HttpResponse {
    HttpResponse::Ok().content_type("text/plain").body("Hello, World!")
}

async fn json() -> HttpResponse {
    HttpResponse::Ok()
        .content_type("application/json")
        .body("{\"message\":\"Hello, World!\"}")
}

async fn user(id: web::Path<String>) -> HttpResponse {
    HttpResponse::Ok().content_type("text/plain").body(id.into_inner())
}

#[actix_web::main]
async fn main() -> std::io::Result<()> {
    let port: u16 = std::env::var("PORT")
        .ok()
        .and_then(|s| s.parse().ok())
        .unwrap_or(8080);
    HttpServer::new(|| {
        App::new()
            .route("/plaintext", web::get().to(plaintext))
            .route("/json", web::get().to(json))
            .route("/user/{id}", web::get().to(user))
    })
    .bind(("127.0.0.1", port))?
    .run()
    .await
}
