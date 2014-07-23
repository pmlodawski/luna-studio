require 'json'

class Dispatcher
    def self.dispatch(user, message)
        case
        when message.topic == "shell_input"
            user.shell.push(message.data)
        when message.topic == "spawn_shell"
            user.spawn_shell(message.data)
        when message.topic == "inotify_subscribe"
            user.inotify_subscribe(message.data)
        when message.topic == "ping"
            user.ws.send({:topic => "pong"}.to_json)
        else
            user.ws.send({:topic => "error", :data => "Unknown topic"}.to_json)
        end
    end
end
