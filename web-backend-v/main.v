module main

import veb

import renderer
import existdb

fn main() {
  db := existdb.new_existdb(existdb.Config{
    baseurl: 'http://localhost:8080'
    username: 'admin'
    password: ''
    collection: 'dmarc'
  })

  mut app := &renderer.App{
    db
  }

  veb.run[renderer.App, renderer.Context](mut app, 8081)
}

