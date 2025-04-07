module main

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

@['/api/dmarc/query'; get]
pub fn (app &App) dmarc_query(mut ctx Context) veb.Result {
  results := User{
    '123'
    'Doe'
    'John'
  }
  return ctx.json(results)
}

pub fn (app &App) index(mut ctx Context) veb.Result {
  message := 'Hello world from veb!'
  return $veb.html()
}

