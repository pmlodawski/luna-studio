#include <string>
#include <iostream>

#include "zmq.hpp"
#include "generated/server-api.pb.h"

using namespace generated::proto;



int main ()
{
    //  Prepare our context and socket
    zmq::context_t context (1);
    zmq::socket_t socket (context, ZMQ_REQ);

    std::cout << "Connecting to server..." << std::endl;
    socket.connect ("tcp://localhost:30521");

    {
        Method method;
        method.set_name(Method_Name_Initialize);

        Maintenance_Initialize_Args args;

        std::string buffer = method.SerializeAsString();
        args.AppendToString(&buffer);
        zmq::message_t request(buffer.size());

        memcpy ((void *) request.data(), buffer.data(), buffer.size());
        socket.send (request);
        
        zmq::message_t reply;
        socket.recv (&reply);
    }

    for(int i = 0 ; i < 10 ; ++i)
    {
        Method method;
        method.set_name(Method_Name_Ping);

        Maintenance_Ping_Args args;

        std::string buffer = method.SerializeAsString();
        args.AppendToString(&buffer);
        zmq::message_t request(buffer.size());

        memcpy ((void *) request.data(), buffer.data(), buffer.size());
        socket.send (request);
        
        zmq::message_t reply;
        socket.recv (&reply);
    }

    {
        Method method;
        method.set_name(Method_Name_Dump);

        Maintenance_Dump_Args args;

        std::string buffer = method.SerializeAsString();
        args.AppendToString(&buffer);
        zmq::message_t request(buffer.size());

        memcpy ((void *) request.data(), buffer.data(), buffer.size());
        socket.send (request);
        
        zmq::message_t reply;
        socket.recv (&reply);
    }

    {
        Method method;
        method.set_name(Method_Name_Shutdown);

        Maintenance_Shutdown_Args args;

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
