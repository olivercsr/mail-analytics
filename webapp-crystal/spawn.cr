num = 100000

channel = Channel(Nil).new

num.times do |i|
  spawn do
    #puts "fiber #{i} start"
    sleep 10.seconds
    #puts "fiber #{i} end"
    channel.send(nil)
  end
end

num.times do |i|
  channel.receive
end

#sleep 11.seconds
#Fiber.yield

