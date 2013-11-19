#include <string>
#include <iostream>

#include "zmq.hpp"
#include "generated/server-api.pb.h"

using namespace generated::proto;
using namespace generated::proto::batch;



int main ()
{
    //  Prepare our context and socket
    zmq::context_t context (1);
    zmq::socket_t socket (context, ZMQ_REQ);

    std::cout << "Connecting to server..." << std::flush;
    socket.connect ("tcp://localhost:30521");
    std::cout << "done" << std::endl;

    std::cout << "Processing requests..." << std::flush;
    {
        Request request;
        request.set_method(Request_Method_Initialize);
        Maintenance_Initialize_Args* args = request.MutableExtension(Maintenance_Initialize_Args::req);

        std::string buffer = request.SerializeAsString();
        zmq::message_t zmq_req(buffer.size());
        memcpy ((void *) zmq_req.data(), buffer.data(), buffer.size());
        socket.send (zmq_req);
        
        zmq::message_t reply;
        socket.recv (&reply);
    }

    for(int i = 0 ; i < 10000 ; ++i)
    {
        Request request;
        request.set_method(Request_Method_Ping);
        Maintenance_Ping_Args* args = request.MutableExtension(Maintenance_Ping_Args::req);

        std::string buffer = request.SerializeAsString();
        zmq::message_t zmq_req(buffer.size());
        memcpy ((void *) zmq_req.data(), buffer.data(), buffer.size());
        socket.send (zmq_req);
        
        zmq::message_t reply;
        socket.recv (&reply);
    }

    {
        Request request;
        request.set_method(Request_Method_Dump);
        Maintenance_Dump_Args* args = request.MutableExtension(Maintenance_Dump_Args::req);

        std::string buffer = request.SerializeAsString();
        zmq::message_t zmq_req(buffer.size());
        memcpy ((void *) zmq_req.data(), buffer.data(), buffer.size());
        socket.send (zmq_req);
        
        zmq::message_t reply;
        socket.recv (&reply);
    }

    {
        Request request;
        request.set_method(Request_Method_Shutdown);
        Maintenance_Shutdown_Args* args = request.MutableExtension(Maintenance_Shutdown_Args::req);

        std::string buffer = request.SerializeAsString();
        zmq::message_t zmq_req(buffer.size());
        memcpy ((void *) zmq_req.data(), buffer.data(), buffer.size());
        socket.send (zmq_req);
        
        zmq::message_t reply;
        socket.recv (&reply);
    }
    std::cout << "done" << std::endl;

    return 0;
}
