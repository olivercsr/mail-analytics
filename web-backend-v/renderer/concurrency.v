module renderer

import veb

pub type RequestHandler[T] = fn (&T, mut Context)

pub struct Context {
  veb.Context
}

pub fn process_request_concurrently[T](handler RequestHandler[T], app &T, mut ctx Context) veb.Result {
  println('accepting request ${ctx.req.url}')

  ctx.takeover_conn()
  go handler(app, mut ctx)

  println('accepted request ${ctx.req.url}')

  return veb.no_result()
}

