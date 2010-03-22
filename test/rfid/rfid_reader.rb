#!/usr/bin/ruby1.9

require "socket"

puts "Get actual ip"
sock = TCPSocket.new "127.0.0.1", 9990
actual_ip = sock.gets

puts "Connect to cluster"
sock = TCPSocket.new actual_ip, 9993
sock.puts "id 42 4 2"
sock.puts "here 1"
sock.puts "here 2"
sock.puts "here 3"
sock.puts "here 4"
sock.puts "here 5"
sock.puts "here 6"
sleep 1
sock.puts "unhere 5"
sock.puts "unhere 2"
sock.puts "unhere 3"

sleep 1

sock.puts "some incorrect message"

sock.close
