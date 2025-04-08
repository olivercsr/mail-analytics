module existdb

//import time
import net.http

pub struct Config {
pub:
  baseurl string
  username string
  password string
  collection string
}

pub struct ExistDb {
  config Config
}

pub fn new_existdb(config Config) ExistDb {
  return ExistDb{config}
}

pub fn (db ExistDb) query(query string) !string {
  response := http.get('http://localhost:8081')!
  dump(response.body)
  return response.body
}
