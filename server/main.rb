#!/usr/bin/env ruby

require 'bundler/setup'

require 'em-websocket'
require 'json'
require 'ostruct'

require_relative 'watcher.rb'
require_relative 'ghci.rb'

$stdout.sync = true

puts "ReactiveServer is starting up ..."

class User
    attr_accessor :ghci, :inotify

    def initialize
        @ghci = Ghci.new
        @inotify = GlobWatcher.new("project/**/*")
    end

    def uninitialize
        @ghci.kill
    end
end

EM.run do
    @clients = {}

    EM::WebSocket.run(:host => "0.0.0.0", :port => 8080) do |ws|
        include Logging
        @current_user = @clients[ws]

        ws.onopen do |handshake|
            logger.info("User #{ws.hash} has connected")
            logger.debug(handshake.headers["User-Agent"])

            user = User.new

            user.ghci.subscribe do |line|
                ws.send({:topic => "ghci_output", :data => line}.to_json)
            end

            user.inotify.subscribe do |event|
                ws.send({:topic => "inotify", :data => event}.to_json)
            end

            @current_user = user
        end

        ws.onclose do
            logger.info("User #{ws.hash} has disconnected")
            @current_user.uninitialize
            @clients.delete(ws)
        end

        ws.onmessage do |msg|
            logger.info("User #{ws.hash} sent message")
            begin
                message = OpenStruct.new JSON.parse(msg)

                case
                when message.topic == "ghci_input"
                    @current_user.ghci.push(message.data)
                when message.topic == "ping"
                    ws.send({:topic => "pong"}.to_json)
                end

            rescue Exception => e
                logger.error("User #{ws.hash} sent invalid message and caused error: #{e}")
                ws.send({:topic => "error", :data => e}.to_json)
            end
        end
    end
end
