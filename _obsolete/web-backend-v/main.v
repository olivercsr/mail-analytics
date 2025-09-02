module main

import veb

import os
import cli
import app
import existdb

fn run_web(authuser_header string, authuser ?string) ! {
  db := existdb.new_existdb(existdb.Config{
    baseurl: 'http://localhost:8080'
    username: 'admin'
    password: ''
    collection: 'dmarc'
  })

  mut theapp := &app.App{
    db: db
  }
  if userid := authuser {
    theapp.use(handler: app.make_fake_authenticate(userid))
  } else {
    theapp.use(handler: app.make_authenticate(authuser_header))
  }
  theapp.handle_static('static', false)!

  veb.run[app.App, app.Context](mut theapp, 8081)
}

fn main() {
  mut cliapp := cli.Command{
    name:        'dmarc-webapp'
    description: 'dmarc-webapp'
    execute:     fn (cmd cli.Command) ! {
      authuser_header := if auh := cmd.flags.get_string('authuser-header') {
        if auh.len > 0 { auh } else { 'remote-user' }
      } else {
        return err
      }
      authuser := if au := cmd.flags.get_string('static-authuser') {
        if au.len > 0 { au } else { none }
      } else {
        return err
      }

      run_web(authuser_header, authuser)!
      return
    }
    flags:       [
      cli.Flag{
        flag: cli.FlagType.string
        name: 'authuser-header'
        description: 'HTTP header to retrieve the authenticated user from for each request. Default is "remote-user".'
      }
      cli.Flag{
        flag: cli.FlagType.string
        name: 'static-authuser'
        description: 'Augment each incoming request with given authuser instead of retrieving it from HTTP header. Useful for development/debugging. Don\'t use this in productionj.'
      }
    ]
    //commands:    [
    //  cli.Command{
    //    name:    'sub'
    //    execute: fn (cmd cli.Command) ! {
    //      println('hello subcommand')
    //      return
    //    }
    //  },
    //]
  }

  cliapp.setup()
  cliapp.parse(os.args)
}

