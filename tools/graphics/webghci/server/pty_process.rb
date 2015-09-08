require 'eventmachine'
require 'pty'

require_relative 'logger.rb'

class PtyProcess
    include Logging

    attr_reader :proc_stdin, :proc_stdout, :proc_pid
    attr_reader :proc_chan_in, :proc_chan_out
    attr_reader :sid, :watch, :process

    module ProcessHandler
        def initialize(queue)
            @queue = queue
        end

        def notify_readable
            begin
                data = @io.readpartial(1000)
                @queue << data.force_encoding('utf-8').encode
            rescue
                logger.info("Smells like dead")
                self.notify_readable = false
            end
        end
    end

    def initialize(command)
        logger.info("Spawning new instance of \"#{command}\"")
        @proc_stdout, @proc_stdin, @proc_pid = PTY.spawn(command)
        @proc_chan_in  = EM::Channel.new
        @proc_chan_out = EM::Channel.new

        @sid = @proc_chan_in.subscribe {|line| @proc_stdin.print(line)}

        @watch = EM.watch(@proc_stdout, ProcessHandler, @proc_chan_out)
        @watch.notify_readable = true
    end

    def push(data)
        @proc_chan_in << data
    end

    def subscribe(&block)
        @proc_chan_out.subscribe(block)
    end

    def kill
        operation = proc do
            logger.info("Terminating subprocess")
            @proc_chan_in.unsubscribe @sid
            @watch.detach
            @proc_stdin.close
            @proc_stdout.close

            begin
                Process.kill("TERM", @proc_pid)
                begin 
                    Timeout::timeout(1) { Process.wait @proc_pid }
                rescue Timeout::Error
                    logger.warn("Subprocess exceeded termination timeout and needs to be kiled")
                    Process.kill("KILL", @proc_pid)
                end
            rescue PTY::ChildExited
            end
        end

        callback = proc do
            logger.info("Subprocess has been terminated")
        end

        EM.defer(operation, callback)
    end
end
