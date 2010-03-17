#!/usr/bin/ruby1.9

#!/usr/bin/ruby1.9

require "socket"

sock = TCPSocket.new "127.0.0.1", 9990
actual_ip = sock.gets

sock = TCPSocket.new actual_ip, 9994
sock.puts "Hello" # FIXME

puts "Number:"

n = gets

sock.puts "number #{n}"
puts "wait answer"
puts sock.gets
sock.close
