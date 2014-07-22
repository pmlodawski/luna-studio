require 'eventmachine'
require 'pty'

require_relative 'logger.rb'

class Ghci
    include Logging

    attr_accessor :ghci_stdin, :ghci_stdout, :ghci_pid
    attr_accessor :ghci_chan_in, :ghci_chan_out
    attr_accessor :sid, :watch

    module ProcessHandler
        def initialize(queue)
            @queue = queue
        end

        def notify_readable
            data = @io.readpartial(1000)
            @queue << data.force_encoding('utf-8').encode
        end
    end

    def initialize
        logger.info("Spawning new ghci instance")
        @ghci_stdout, @ghci_stdin, @ghci_pid = PTY.spawn("ghci")
        @ghci_chan_in  = EM::Channel.new
        @ghci_chan_out = EM::Channel.new

        @sid = @ghci_chan_in.subscribe {|line| @ghci_stdin.print(line)}

        @watch = EM.watch(@ghci_stdout, ProcessHandler, @ghci_chan_out)
        @watch.notify_readable = true
    end

    def push(data)
        @ghci_chan_in << data
    end

    def subscribe(&block)
        @ghci_chan_out.subscribe(block)
    end

    def kill
        operation = proc do
            logger.info("Terminating ghci instance")
            @ghci_chan_in.unsubscribe @sid
            @watch.detach
            @ghci_stdin.close
            @ghci_stdout.close

            begin
                Process.kill("TERM", @ghci_pid)
                begin 
                    Timeout::timeout(1) { Process.wait @ghci_pid }
                rescue Timeout::Error
                    logger.warn("Ghci exceeded termination timeout and needs to be kiled")
                    Process.kill("KILL", @ghci_pid)
                end
            rescue PTY::ChildExited
            end
        end

        callback = proc do
            logger.info("Ghci instance has been terminated")
        end

        EM.defer(operation, callback)
    end
end
