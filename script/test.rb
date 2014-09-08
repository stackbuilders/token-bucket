#!/usr/bin/env ruby

require 'socket'

PORT            = 4444
HOST            = 'localhost'

BUCKETS         = File.read('config').lines.map(&:split).
                    map{|name, rate| [name, rate.to_i]}

THREADS         = 2
REQS_PER_THREAD = 100_000
SLEEP_INTERVAL  = 0.001

results = [['name',
            'time',
            'granted',
            'rate',
            'goal',
            'dev %']]

BUCKETS.each do |name, rate|
  granted_tokens  = 0

  start_time = Time.now

  workers = (0...THREADS).map do
    Thread.new do
      s = TCPSocket.new HOST, PORT

      REQS_PER_THREAD.times do

        begin
          s.write "get #{name}\r\n"

          res = s.recv(1024).to_i
          if res == 1
            granted_tokens += 1

          elsif res == 0
            # NOOP

          else
            raise "Unknown response: #{res}"

          end

        rescue  Errno::EPIPE => e
          $stderr.puts "Lost connection to socket, reconnecting."
          s = TCPSocket.new HOST, PORT

        rescue Errno::ECONNRESET => e
          $stderr.puts "Connection reset!"

        rescue SocketError => e
          $stderr.puts "Rescued a socket error!"

        end

        sleep SLEEP_INTERVAL
      end

      s.close
    end
  end

  workers.map(&:join)

  end_time = Time.now

  total_time = end_time - start_time
  results << [name,
              total_time,
              granted_tokens,
              granted_tokens / total_time,
              rate,
              - (rate - granted_tokens / total_time) / rate.to_f * 100
             ]
end

puts
puts "*" * 80

puts "## Testing with #{THREADS} threads doing #{REQS_PER_THREAD} " +
     "requests each and a #{SLEEP_INTERVAL} sleep interval.\n\n"

printf "| %10s | %10s | %10s | %10s | %10s | %10s |\n", *results[0]

printf "| %10s | %10s | %10s | %10s | %10s | %10s |\n",
       *['---', '---', '---', '---', '---', '---']

results[1..-1].each do |r|
  printf "| %10s | %10.2f | %10d | %10.2f | %10s | %10.2f |\n", *r
end
puts "*" * 80
