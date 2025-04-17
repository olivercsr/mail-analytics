module renderer

import time
import veb

import existdb

pub struct App {
pub:
  db existdb.ExistDb @[required]
}

struct User {
  id string
  lastname string
  firstname string
}

@['/api/dmarc/query'; get]
pub fn (app &App) dmarc_query(mut ctx renderer.Context) veb.Result {
  handler := fn (app &App, mut context renderer.Context) {
    println('start processing request ${context.req.url}')

    time.sleep(5000 * time.millisecond)

    user := User{
      '123'
      'Doe'
      'John'
    }
    data := {
      'foo': 123
      'bar': 234
    }
    //db_result := app.db.query_row_count() or { println('oops ${err}'); '' }
    db_result := app.db.query_count() or { println('oops ${err}'); ''}
    result := struct {
      user: user
      data: data
      db_result: db_result
    }

    println(typeof(user).name)
    println(typeof(data).name)
    println(typeof(db_result).name)
    println(typeof(result).name)

    println('done processing request ${context.req.url}')

    context.json([result])
  }

  return renderer.process_request_concurrently(handler, app, mut ctx)
}

@[get]
pub fn (app &App) query(mut ctx renderer.Context) veb.Result {
  result := 'abb'
  return $veb.html()
}

@[get]
pub fn (app &App) index(mut ctx renderer.Context) veb.Result {
  context := ctx
  message := 'Hello world from veb!'
  return $veb.html()
}

