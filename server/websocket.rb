require 'logger'
require 'em-websocket'

class Websocket < EM::WebSocket
    include Logging

    def initialize
        @clients = {}
    end

    def onopen(handshake)
        logger.info("User #{ws.hash} has connected")
        logger.debug(handshake.headers["User-Agent"])

        @clients[ws] = Ghci.new
        @clients[ws].subscribe do |line|
            ws.send({:topic => "ghci_output", :data => line}.to_json)
        end

        @inotify_chan.subscribe do |event|
            ws.send({:topic => "inotify", :data => event}.to_json)
        end

        @glob_watcher.notify_existing
    end

    def onclose
        logger.info("User #{ws.hash} has disconnected")
        @clients[ws].kill
        @clients.delete(ws)
    end

    def onmessage(msg)
        logger.info("User #{ws.hash} sent message")
        begin
            message = OpenStruct.new JSON.parse(msg)
            case
            when message.topic == "ghci_input"
                @clients[ws].push(message.data)
            when message.topic == "ping"
                ws.send({:topic => "pong"}.to_json)
            end
        rescue Exception => e
            logger.error("User #{ws.hash} sent invalid message and caused error: #{e}")
            ws.send({:topic => "error", :data => e}.to_json)
        end
    end
end
