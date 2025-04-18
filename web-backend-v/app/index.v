module app

import veb

@[get]
pub fn (theapp &App) index(mut ctx app.Context) veb.Result {
  userid := ctx.userid
  message := 'Hello world from veb!'
  context := ctx
  return $veb.html()
}


