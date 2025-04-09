module renderer

import time
import veb

pub struct App {}

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

  return renderer.process_request_concurrently(handler, app, mut ctx)
}

pub fn (app &App) index(mut ctx renderer.Context) veb.Result {
  message := 'Hello world from veb!'
  return $veb.html()
}

