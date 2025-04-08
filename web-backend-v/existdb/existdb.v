module existdb

//import time
import net.http

const api_path := '/exist/rest'

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
  url := '${db.config.baseurl}${api_path}/${db.config.collection}'
  response := http.get(url)!

  return response.body
}
