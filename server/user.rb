require 'json'

require_relative 'watcher.rb'
require_relative 'ghci.rb'

class User
    attr_reader :ws, :ghci, :inotify

    def initialize(ws)
        @ws = ws
        @ghci = Ghci.new

        @ghci.subscribe do |line|
            @ws.send({:topic => "ghci_output", :data => line}.to_json)
        end
    end

    def inotify_subscribe(path)
        @inotify.uninitialize if @inotify
        @inotify = GlobWatcher.new(path) 
        @inotify.subscribe do |event|
            @ws.send({:topic => "inotify", :data => event}.to_json)
        end
    end

    def uninitialize
        @ghci.kill
        @inotify.uninitialize unless @inotify.nil?
    end
end
