
#ifdef _WIN32
	#pragma comment(lib, "libprotobuf.lib")
	#define _WIN32_WINNT 0x501 //Needs to come before asio.hpp
#endif


#include <chrono>
#include <iostream>
#include <string>
#include <algorithm>

#include <google/protobuf/io/zero_copy_stream_impl.h>
#include <google/protobuf/io/coded_stream.h>
// zero_copy_stream_impl.h>
#include "generated/server-api.pb.h"


#include <boost/asio.hpp>
using boost::asio::ip::tcp;
using Socket = tcp::socket;

using namespace google::protobuf::io;

using namespace generated::proto;
using namespace generated::proto::batch;

const int BUFFER_SIZE = 10000000; // TODO [PM] : magic constant
char buffer[BUFFER_SIZE];

class StopWatch
{
	typedef std::chrono::high_resolution_clock clock;
	typedef std::chrono::microseconds microseconds;
	typedef std::chrono::milliseconds milliseconds;

	milliseconds intervalMs(const clock::time_point& t1, const clock::time_point& t0)
	{
		return std::chrono::duration_cast<milliseconds>(t1 - t0);
	}

	clock::time_point start_;
public:
	StopWatch() : start_(clock::now()) {}
	clock::time_point restart() { start_ = clock::now(); return start_; }
	milliseconds elapsedMs()
	{
		return std::chrono::duration_cast<milliseconds>(clock::now() - start_);
	}
};

size_t sendAll(Socket &socket, void *data, size_t size)
{
	size_t sent = boost::asio::write(socket, boost::asio::buffer(data, size));
	assert(sent == size);
	return sent;
}


void sendRequest(Socket &socket, const Request& request)
{
	int  requestSize = request.ByteSize() + 4;
	char* requestBuf = new char[requestSize];

	//write varint delimiter to buffer
	ArrayOutputStream arrayOut(requestBuf, requestSize);
	CodedOutputStream codedOut(&arrayOut);
	codedOut.WriteVarint32(request.ByteSize());

	//write protobuf ack to buffer
	request.SerializeToCodedStream(&codedOut);

	//send buffer to client
	sendAll(socket, requestBuf, requestSize);
	// std::cout << "Sent: " << sent << std::flush;

	delete [] requestBuf;
}

Response receiveResponse(Socket &socket)
{
	Response response;
	size_t received = boost::asio::read(socket, boost::asio::buffer(buffer, 4));

	//read varint delimited protobuf object in to buffer
	//there's no method to do this in the C++ library so here's the workaround
	ArrayInputStream headerArrayIn(buffer, received);
	CodedInputStream headerCodedIn(&headerArrayIn);
	google::protobuf::uint32 packetSize;
	headerCodedIn.ReadVarint32(&packetSize);
	const int sizeinfoLength = headerCodedIn.CurrentPosition();
	const int remainingToRead = packetSize + sizeinfoLength - received;

	received = boost::asio::read(socket, boost::asio::buffer(buffer + received, remainingToRead));

	ArrayInputStream arrayIn(buffer + sizeinfoLength, packetSize);
	CodedInputStream codedIn(&arrayIn);
	CodedInputStream::Limit msgLimit = codedIn.PushLimit(packetSize);
	response.ParseFromCodedStream(&codedIn);
	codedIn.PopLimit(msgLimit);
	return response;
}

Response call(Socket &socket, const Request& request)
{
	sendRequest(socket, request);
	return receiveResponse(socket);
}

int main()
{
	try
	{
		//Prepare Boost.Asio
		boost::asio::io_service io_service;
		tcp::socket socket(io_service);
		tcp::resolver resolver(io_service);
		//Connect
		boost::asio::connect(socket, resolver.resolve({ "localhost", "30521" }));

		StopWatch sw;
		std::cout << "Processing requests.." << std::flush;
		{
			Request request;
			request.set_method(Request_Method_Maintenance_Initialize);
			Maintenance_Initialize_Args* args = request.MutableExtension(Maintenance_Initialize_Args::req);

			// request.SerializeToFileDescriptor(sockfd);
			// output.WriteVarint32(request.ByteSize());
			// request.SerializeToZeroCopyStream(&foutput);
			// request.SerializeToCodedStream(&output);
			// foutput.Flush();

			// std::string buffer = request.SerializeAsString();
			// int l = send(sockfd, buffer.c_str(), (size_t) buffer.size() + 5, 0);
			// std::cout << "sent " << l << std::flush;
			// if (-1 == l) {
			//     perror("send");
			//     return 1;
			// }
			// std::cout << "sent" << std::flush;

			Response response = call(socket, request);

			// response.ParseFromZeroCopyStream(input);
			// response.ParseFromFileDescriptor(sockfd);
			// std::cout << "received" << std::flush;

			// char b[1000];
			// int r = recv(sockfd, b, 1000, 0);
			// std::cout << "received " << r << std::flush;
			// zmq::message_t reply;
			// socket.recv (&reply);
		}

		std::cout << "." << std::endl;
		std::cout << "Request-response: " << sw.elapsedMs().count() << "ms\n";
		const int pingCount = 10000;
		for(int i = 0; i < pingCount; ++i)
		{
			Request request;
			request.set_method(Request_Method_Maintenance_Ping);
			Maintenance_Ping_Args* args = request.MutableExtension(Maintenance_Ping_Args::req);

			Response response = call(socket, request);
		}
		{
			Request request;
			request.set_method(Request_Method_Maintenance_Dump);
			Maintenance_Dump_Args* args = request.MutableExtension(Maintenance_Dump_Args::req);

			Response response = call(socket, request);
		}

		std::cout << pingCount << " pings " << sw.elapsedMs().count() << "ms\n";

		const int ps = 100000000;
		{
			Request request;
			request.set_method(Request_Method_Maintenance_Ping);
			Maintenance_Ping_Args* args = request.MutableExtension(Maintenance_Ping_Args::req);

			char* p = new char[ps];
			std::fill(p, p + ps - 1, 65);
			p[ps - 1] = 0;
			args->set_data(p);
			delete [] p;
			Response response = call(socket, request);
		}
		{
			Request request;
			request.set_method(Request_Method_Maintenance_Shutdown);
			Maintenance_Shutdown_Args* args = request.MutableExtension(Maintenance_Shutdown_Args::req);
			Response response = call(socket, request);
		}
		std::cout << "Sending " << ps << " bytes " << sw.elapsedMs().count() << "ms\n";
		std::cout << "done" << std::endl;

		return 0;
	}
	catch(std::exception &e)
	{
		std::cout << "Encountered an exception: " << e.what() << std::endl;
		return 1;
	}
}
