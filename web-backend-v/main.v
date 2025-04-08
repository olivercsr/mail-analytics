module main

import time
import veb

pub struct Context {
  veb.Context
}

pub struct App {}

struct User {
  id string
  lastname string
  firstname string
}

fn main() {
  mut app := &App{}
  veb.run[App, Context](mut app, 8081)
}

fn (app &App) do_dmarc_query(mut ctx Context) veb.Result {
  println('start processing request ${ctx.req.url}')

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

  println('done processing request ${ctx.req.url}')

  return ctx.json([result])
}

@['/api/dmarc/query'; get]
pub fn (app &App) dmarc_query(mut ctx Context) veb.Result {
  println('accepting request ${ctx.req.url}')

  ctx.takeover_conn()
  go app.do_dmarc_query(mut ctx)

  println('accepted request ${ctx.req.url}')

  return veb.no_result()
}

pub fn (app &App) index(mut ctx Context) veb.Result {
  message := 'Hello world from veb!'
  return $veb.html()
}

