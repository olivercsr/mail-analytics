module app

import time
import veb

@['/api/dmarc/query'; get]
pub fn (theapp &App) dmarc_query(mut ctx app.Context) veb.Result {
  handler := fn (theapp &App, mut context app.Context) {
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
    //db_result := theapp.db.query_row_count() or { println('oops ${err}'); '' }
    db_result := theapp.db.query_count() or { println('oops ${err}'); ''}
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

  return app.process_request_concurrently(handler, theapp, mut ctx)
}

@[get]
pub fn (theapp &App) query(mut ctx app.Context) veb.Result {
  result := 'abb'
  return $veb.html()
}

