module app

import veb
import net.http

import existdb

pub type RequestHandler[T, U] = fn (&T, mut U)

fn handler_wrapper[T, U](handler RequestHandler[T, U], theapp &T, mut ctx U) {
  println('calling handler...')
  handler(theapp, mut ctx)
}

pub fn process_request_concurrently[T, U](handler RequestHandler[T, U], theapp &T, mut ctx U) veb.Result {
  println('accepting request ${ctx.req.url}')

  ctx.takeover_conn()
  go handler_wrapper(handler, theapp, mut ctx)

  println('accepted request ${ctx.req.url}')

  return veb.no_result()
}

struct User {
  id string
  lastname string
  firstname string
}

pub struct Context {
  veb.Context
pub mut:
  userid string
}

pub struct App {
  veb.Middleware[Context]
  veb.StaticHandler
pub:
  db existdb.ExistDb @[required]
}

pub fn make_authenticate(http_header string) fn(mut app.Context) bool {
  return fn [http_header] (mut ctx Context) bool {
    userid := ctx.get_custom_header(http_header) or { ctx.server_error_with_status(http.Status.unauthorized); '' }
    if check_userid(userid) {
      ctx.userid = userid
      return true
    } else {
      ctx.res.set_status(http.Status.unauthorized)
      return false
    }
  }
}

pub fn make_fake_authenticate(userid string) fn(mut app.Context) bool {
  return fn [userid] (mut ctx Context) bool {
    ctx.userid = userid
    return true
  }
}
