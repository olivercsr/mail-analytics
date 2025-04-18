module main

import veb

import app
import existdb

fn main() {
  db := existdb.new_existdb(existdb.Config{
    baseurl: 'http://localhost:8080'
    username: 'admin'
    password: ''
    collection: 'dmarc'
  })

  mut theapp := &app.App{
    db: db
  }
  theapp.use(handler: app.authenticate)

  veb.run[app.App, app.Context](mut theapp, 8081)
}

