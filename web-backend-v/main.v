module main

import time
import veb

import existdb

type RequestHandler = fn (&App, mut Context)

pub struct Context {
  veb.Context
}

pub struct App {}

struct User {
  id string
  lastname string
  firstname string
}

fn process_request_concurrently(handler RequestHandler, app &App, mut ctx Context) veb.Result {
  println('accepting request ${ctx.req.url}')

  ctx.takeover_conn()
  go handler(app, mut ctx)

  println('accepted request ${ctx.req.url}')

  return veb.no_result()
}

@['/api/dmarc/query'; get]
pub fn (app &App) dmarc_query(mut ctx Context) veb.Result {

  handler := fn (app &App, mut context Context) {
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
    result := struct {
    user: user
    data: data
  }

    println(typeof(user).name)
    println(typeof(data).name)
    println(typeof(result).name)

    println('done processing request ${context.req.url}')

    context.json([result])
  }

  return process_request_concurrently(handler, app, mut ctx)
}

pub fn (app &App) index(mut ctx Context) veb.Result {
  message := 'Hello world from veb!'
  return $veb.html()
}

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

  mut app := &App{}
  veb.run[App, Context](mut app, 8081)
}

