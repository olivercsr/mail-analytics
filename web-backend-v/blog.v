module main

import veb

pub struct Context {
  veb.Context
}

pub struct App {}

fn main() {
  mut app := &App{}
  veb.run[App, Context](mut app, 8081)
}

pub fn (app &App) index(mut ctx Context) veb.Result {
  message := 'Hello world from veb!'
  return $veb.html()
}

