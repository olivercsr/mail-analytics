module renderer

import veb

pub type RequestHandler[T, U] = fn (&T, mut U)

pub fn process_request_concurrently[T, U](handler RequestHandler[T, U], app &T, mut ctx U) veb.Result {
  println('accepting request ${ctx.req.url}')

  ctx.takeover_conn()
  go handler(app, mut ctx)

  println('accepted request ${ctx.req.url}')

  return veb.no_result()
}

