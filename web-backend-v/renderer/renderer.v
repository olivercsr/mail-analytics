module renderer

import time
import veb
import net.http

import auth
import existdb

pub struct Context {
  veb.Context
pub mut:
  userid string
}

pub struct App {
  veb.Middleware[Context]
pub:
  db existdb.ExistDb @[required]
}

struct User {
  id string
  lastname string
  firstname string
}

pub fn authenticate(mut ctx Context) bool {
  userid := ctx.get_custom_header('remote-user') or { ctx.server_error_with_status(http.Status.unauthorized); '' }
  if auth.check_userid(userid) {
    ctx.userid = userid
    return true
  } else {
    ctx.server_error_with_status(http.Status.unauthorized)
    return false
  }
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
  userid := ctx.userid
  message := 'Hello world from veb!'
  context := ctx
  return $veb.html()
}

