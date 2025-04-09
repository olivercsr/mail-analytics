module main

import veb

import renderer
import existdb

fn test_existdb() {
  db := existdb.new_existdb(existdb.Config{
    baseurl: 'http://localhost:8080'
    username: 'admin'
    password: ''
    collection: 'dmarc'
  })
  body := db.query('foo') or { println('oops'); println(err); '' }
  println('=================')
  println('response body:\n' + body)
}

fn main() {
  test_existdb()

  mut app := &renderer.App{}
  veb.run[renderer.App, renderer.Context](mut app, 8081)
}

