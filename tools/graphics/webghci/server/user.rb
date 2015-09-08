require 'json'
require 'mimemagic'

require_relative 'watcher.rb'
require_relative 'pty_process.rb'

class User
    attr_reader :ws, :shell, :inotify

    def initialize(ws)
        @ws = ws
        @inotify = Watcher.new
    end

    def spawn_shell(command)
        @shell.kill unless @shell.nil?

        @shell = PtyProcess.new(command)
        @shell.subscribe do |line|
            @ws.send({:topic => "shell_output", :data => line}.to_json)
        end
    end

    def inotify_subscribe(path)
        Dir.chdir(path)
        Dir["**/*"].each do |file|
            ev = { :name => File.join(path, file),
                   :mime => MimeMagic.by_path(file),
                   :size => File.size?(file),
                   :flags => [:create]
                 }
            ev[:flags] << :isdir if File.directory?(file)

            @ws.send({:topic => "inotify", :data => ev}.to_json)
        end
        Dir.chdir(File.dirname(__FILE__))

        @inotify.subscribe(path) do |event|
            ev = { :name => event.absolute_name,
                   :mime => MimeMagic.by_path(event.absolute_name),
                   :size => event.size, # or File.size(event.absolute_name)
                   :flags => event.flags
                }
            @ws.send({:topic => "inotify", :data => ev}.to_json)
        end
    end

    def inotify_cancel(path)
        @inotify.cancel(path)
        
        Dir.chdir(path)
        Dir["**/*"].each do |file|
            ev = { :name => File.join(path, file),
                   :mime => MimeMagic.by_path(file),
                   :size => File.size?(file),
                   :flags => [:delete]
                 }
            ev[:flags] << :isdir if File.directory?(file)

            @ws.send({:topic => "inotify", :data => ev}.to_json)
        end
        Dir.chdir(File.dirname(__FILE__))
    end

    def uninitialize
        @shell.kill unless @shell.nil?
        @inotify.kill unless @inotify.nil?
    end
end
