require 'json'

require_relative 'watcher.rb'
require_relative 'pty_process.rb'

class User
    attr_reader :ws, :shell, :inotify

    def initialize(ws)
        @ws = ws
    end

    def spawn_shell(command)
        @shell.kill unless @shell.nil?

        @shell = PtyProcess.new(command)
        @shell.subscribe do |line|
            @ws.send({:topic => "shell_output", :data => line}.to_json)
        end
    end

    def inotify_subscribe(path)
        @inotify.uninitialize unless @inotify.nil?

        @inotify = GlobWatcher.new(path) 
        @inotify.subscribe do |event|
            @ws.send({:topic => "inotify", :data => event}.to_json)
        end
    end

    def uninitialize
        @shell.kill unless @shell.nil?
        @inotify.uninitialize unless @inotify.nil?
    end
end
