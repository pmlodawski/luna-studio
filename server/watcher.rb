require 'rb-inotify'
require 'eventmachine'

require_relative 'logger.rb'

class Watcher
    include Logging

    def initialize
        @notifier = INotify::Notifier.new

        @watch = EM.watch @notifier.to_io do |conn|
            class << conn
                attr_accessor :notifier

                def notify_readable
                    @notifier.process
                end
            end
            conn.notifier = @notifier
            conn.notify_readable = true
        end
    end

    def subscribe(path, &block)
        logger.info("Establishing inotify watches on \"#{path}\"")
        @notifier.watch(path, :all_events, :recursive, &block)
    end

    def cancel(path)
        logger.info("Canceling inotify watches on \"#{path}\"")
        raise "Not implemented yet"
    end

    def kill
        logger.info("Killing inotify")
        @notifier.stop
        @notifier.close
        @watch.detach
        rescue SystemCallError
    end
end
