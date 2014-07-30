#!/usr/bin/env ruby

require 'bundler/setup'

require 'em-websocket'
require 'rubygems'
require 'json'
require 'ostruct'

require_relative 'http.rb'
require_relative 'logger.rb'
require_relative 'dispatcher.rb'
require_relative 'user.rb'

$stdout.sync = true

puts "ReactiveServer is starting up ..."

EM.kqueue if EM.kqueue?
EM.epoll  if EM.epoll?

EM.run do
    Signal.trap("INT")  { EventMachine.stop }
    Signal.trap("TERM") { EventMachine.stop }
    
    @clients = {}
    EM::start_server("0.0.0.0", 8000, HTTPHandler)
    EM::WebSocket.run(:host => "0.0.0.0", :port => 8080) do |ws|
        include Logging

        ws.onopen do |handshake|
            logger.info("User #{ws.hash} has connected")
            logger.debug(handshake.headers["User-Agent"])
            @clients[ws] = User.new ws
        end

        ws.onclose do
            logger.info("User #{ws.hash} has disconnected")
            @clients[ws].uninitialize
            @clients.delete(ws)
        end

        ws.onmessage do |msg|
            logger.info("User #{ws.hash} sent message")
            begin
                message = OpenStruct.new JSON.parse(msg)
                Dispatcher.dispatch(@clients[ws], message)
            rescue Exception => e
                logger.error("User #{ws.hash} sent invalid message and caused error: #{e}")
                ws.send({:topic => "error", :data => e}.to_json)
            end
        end
    end
end
