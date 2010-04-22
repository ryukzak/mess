#!/usr/bin/ruby1.9

require "socket"

actual_ip = nil

range = 0..10
times = 20
random_limit = 40

list = []

range.each do |n|
  sock = TCPSocket.new actual_ip, 9994
  sock.puts "id #{n}"
  list[n] = sock
end

times.times {list.each {|s| s.puts "number #{1 + (rand random_limit)}"}; sleep 1}

list.each {|e| e.close}

puts "complete"
