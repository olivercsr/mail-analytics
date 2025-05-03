require "http/server"

server = HTTP::Server.new do |context|
  sleep 5.seconds
  context.response.content_type = "text/plain"
  context.response.print "Hello world! The time is #{Time.local}"
end

address = server.bind_tcp 8081
puts "Listening on http://#{address}"
server.listen

