

#include <arpa/inet.h>
#include <iostream>
#include <netdb.h>
#include <netinet/in.h>
#include <stdio.h>
#include <string>
#include <sys/types.h>
#include <sys/socket.h>
#include <unistd.h>

#include <google/protobuf/io/zero_copy_stream_impl.h>
// zero_copy_stream_impl.h>
#include "generated/server-api.pb.h"

using namespace google::protobuf::io;

using namespace generated::proto;
using namespace generated::proto::batch;



int main ()
{
    //  Prepare our context and socket
    int sockfd;
    struct sockaddr_in server;
    struct addrinfo *serverinfo;
    ssize_t bytesreceived = 0;

    sockfd = socket(AF_INET, SOCK_STREAM, 0);
    if (-1 == sockfd) {
        perror("socket");
        return 1;
    }

    if (0 != getaddrinfo("localhost", NULL, NULL, &serverinfo)) {
        perror("getaddrinfo");
        return 1;
    }

    /*Copy size of sockaddr_in b/c res->ai_addr to big for this example*/
    memcpy(&server, serverinfo->ai_addr, sizeof(struct sockaddr_in));
    server.sin_family = AF_INET;
    server.sin_port = htons(30521);
    freeaddrinfo(serverinfo);

    std::cout << "Connecting to server..." << std::flush;
    if (-1 == connect(sockfd, (const struct sockaddr *) &server,
            sizeof(struct sockaddr_in))) {
        perror("connect");
        return 1;
    }

    std::cout << "done" << std::endl;

    // FileInputStream*  input  = new FileInputStream(sockfd);
    // FileOutputStream* output = new FileOutputStream(sockfd);

    std::cout << "Processing requests..." << std::flush;
    {
        Request request;
        request.set_method(Request_Method_Initialize);
        Maintenance_Initialize_Args* args = request.MutableExtension(Maintenance_Initialize_Args::req);

        request.SerializeToFileDescriptor(sockfd);
        // request.SerializeToZeroCopyStream(output);
        // output->Flush();

        // std::string buffer = request.SerializeAsString();
        // int l = send(sockfd, buffer.c_str(), (size_t) buffer.size() + 5, 0);
        // std::cout << "sent " << l << std::flush;
        // if (-1 == l) {
        //     perror("send");
        //     return 1;
        // }
        std::cout << "sent" << std::flush;

        Response response;
        // response.ParseFromZeroCopyStream(input);
        response.ParseFromFileDescriptor(sockfd);
        std::cout << "received" << std::flush;

        // char b[1000];
        // int r = recv(sockfd, b, 1000, 0);
        // std::cout << "received " << r << std::flush;
        // zmq::message_t reply;
        // socket.recv (&reply);
    }
    // for(int i = 0 ; i < 10000 ; ++i)
    // {
    //     Request request;
    //     request.set_method(Request_Method_Ping);
    //     Maintenance_Ping_Args* args = request.MutableExtension(Maintenance_Ping_Args::req);

    //     std::string buffer = request.SerializeAsString();
    //     zmq::message_t zmq_req(buffer.size());
    //     memcpy ((void *) zmq_req.data(), buffer.data(), buffer.size());
    //     socket.send (zmq_req);
        
    //     zmq::message_t reply;
    //     socket.recv (&reply);
    // }

    // {
    //     Request request;
    //     request.set_method(Request_Method_Dump);
    //     Maintenance_Dump_Args* args = request.MutableExtension(Maintenance_Dump_Args::req);

    //     std::string buffer = request.SerializeAsString();
    //     zmq::message_t zmq_req(buffer.size());
    //     memcpy ((void *) zmq_req.data(), buffer.data(), buffer.size());
    //     socket.send (zmq_req);
        
    //     zmq::message_t reply;
    //     socket.recv (&reply);
    // }

    // {
    //     Request request;
    //     request.set_method(Request_Method_Shutdown);
    //     Maintenance_Shutdown_Args* args = request.MutableExtension(Maintenance_Shutdown_Args::req);

    //     std::string buffer = request.SerializeAsString();
    //     zmq::message_t zmq_req(buffer.size());
    //     memcpy ((void *) zmq_req.data(), buffer.data(), buffer.size());
    //     socket.send (zmq_req);
        
    //     zmq::message_t reply;
    //     socket.recv (&reply);
    // }
    std::cout << "done" << std::endl;

    return 0;
}
