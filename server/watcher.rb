require 'eventmachine-tail'
require 'eventmachine'

require_relative 'logger.rb'

class Handler < EventMachine::FileWatch
    include Logging

    def initialize(chan)
        @chan = chan
    end

    def file_modified
        logger.debug("#{path} file modified")
        @chan << {:event => "modify", :path => self.path}
    end

    def file_moved
        logger.debug("#{path} file moved")
        @chan << {:event => "move", :path => self.path}
    end

    def file_deleted
    end
end

class GlobWatcher < EventMachine::FileGlobWatch
    include Logging

    def initialize(pathglob, interval=1)
        logger.info("Establishing inotify watches on \"#{pathglob}\"")
        @chan = EM::Channel.new
        @pathglob = pathglob
        Dir[pathglob].each do |file|
            EM.watch_file(file, Handler, @chan)
        end

        super(pathglob, interval)
    end

    def file_deleted(path)
        logger.debug("#{path} file deleted")
        @chan << {:event => "delete", :path => path}
    end

    def file_found(path)
        logger.debug("#{path} file created")
        @chan << {:event => "create", :path => path}
        EM.watch_file(path, Handler, @chan)
    end

    def subscribe(&block)
        @chan.subscribe(block)
    end
end
