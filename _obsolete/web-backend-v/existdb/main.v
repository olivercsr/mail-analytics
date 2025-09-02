module existdb

import os
//import time
import net.http
import encoding.xml

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

pub fn (db ExistDb) build_query(template_name string) !string {
  header := os.read_file("existdb/templates/header.xml")!
  footer := os.read_file("existdb/templates/footer.xml")!
  body := os.read_file('existdb/templates/${template_name}.xquery')!
  return '${header}${body}${footer}'
}

pub fn (db ExistDb) do_request(query string) !http.Response {
  url := '${db.config.baseurl}${api_path}/${db.config.collection}'

  println('=====================================')
  println(query)
  println('=====================================')

  return http.post(url, query)!
}

pub fn (db ExistDb) query_row_count() !string {
  query := db.build_query('query_row_count')!
  response := db.do_request(query)!

  return response.body
}

pub fn (db ExistDb) query_count() !string {
  query := db.build_query('query_count')!
  response := db.do_request(query)!

  return response.body
}

