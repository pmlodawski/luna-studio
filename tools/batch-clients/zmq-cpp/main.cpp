#include <zmq.hpp>
#include <string>
#include <iostream>

#include "generated/server-api.pb.h"

#define call(socket, name, args) {\
        auto size = name.size(); \
        zmq::message_t request_method(size); \
        memcpy ((void*) request_method.data(), name, size); \
        socket.send(request_method); \
    }


int main ()
{
    //  Prepare our context and socket
    zmq::context_t context (1);
    zmq::socket_t socket (context, ZMQ_REQ);

    std::cout << "Connecting to server..." << std::endl;
    socket.connect ("tcp://localhost:30521");

    for(int i = 0 ; i < 100000 ; ++i)
    {
        Server_Method method;
        method.set_name(Server_Method_Name_PING);

        Server_Ping_Args args;
        args.set_val(i);

        std::string buffer = method.SerializeAsString();
        args.AppendToString(&buffer);
        zmq::message_t request(buffer.size());

        memcpy ((void *) request.data(), buffer.data(), buffer.size());
        socket.send (request);
        
        zmq::message_t reply;
        socket.recv (&reply);
    }
    std::cout << "done" << std::endl;

    return 0;
}
