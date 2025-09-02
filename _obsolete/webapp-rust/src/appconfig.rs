use std::env;
use std::error::Error;

#[derive(Debug, Clone)]
pub struct AppConfig {
    pub loglevel: String,
    pub host: String,
    pub port: u16,
    pub existdb_uri: String,
    pub auth_userheader: String,
    pub dev_authuser: Option<String>,
}

pub fn read_appconfig() -> Result<AppConfig, Box<dyn Error>> {
    Ok(AppConfig {
        loglevel: env::var("LOGLEVEL").unwrap_or_else(|_| String::from("info")),
        host: env::var("HOST").unwrap_or_else(|_| String::from("0.0.0.0")),
        port: env::var("PORT").unwrap_or_else(|_| String::from("8080")).parse()?,
        existdb_uri: String::from("http://localhost:8080/exist/rest/dmarc"),
        auth_userheader: String::from("remote-user"),
        dev_authuser: None,
    })
}

