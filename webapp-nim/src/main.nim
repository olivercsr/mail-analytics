import std/os
#import std/asyncdispatch
#import std/asynchttpserver
import prologue

type Result[T, E] = object
  case ok: bool
  of true: value: T
  of false: error: E

proc foo(n: int): Result[int, string] =
  echo("well what")
  #return Result[int, string](ok: true, value: 11)
  if n>10:
    return Result[int, string](ok: false, error: "just an error")

proc hello*(ctx: Context) {.async.} =
  sleep(5000)
  resp "<h1>Hello Prologue!</h1>"

proc prologue_main =
  var i = 0
  let res = foo(11)
  echo type res
  echo res
  i = res.value
  let settings = newSettings(debug = true, port = Port(8081))
  let app = newApp(settings = settings)
  app.addRoute("/", hello)
  app.run()

#proc main {.async.} =
#  let server = newAsyncHttpServer()
#
#  proc cb(req: asynchttpserver.Request) {.async.} = 
#    echo (req.reqMethod, req.url, req.headers)
#    let headers = {"Content-type": "text/plain; charset=utf-8"}
#    await req.respond(Http200, "Hello World", headers.newHttpHeaders())
#
#  server.listen(Port(8081))
#  let port = server.getPort()
#  echo "test this with: curl http://locahost:" & $port.uint16 & "/"
#  while true:
#    if server.shouldAcceptRequest():
#      await server.acceptRequest(cb)
#    else:
#      await sleepAsync(500)
#
#waitFor main()
prologue_main()

