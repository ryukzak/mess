#!/usr/bin/ruby1.9

require "socket"

actual_ip = nil
sock = nil

begin
  puts "Get actual ip"
  sock = TCPSocket.new "127.0.0.1", 9990
rescue
  puts "Error: #{$!}"
else
  actual_ip = sock.gets
end

begin 
  puts "Connect to cluster"
  sock = TCPSocket.new actual_ip, 9993
  sock.puts "id 0"
  sock.puts "number 35"
rescue
  puts "Error: #{$!}"
else
  sock.close
end
