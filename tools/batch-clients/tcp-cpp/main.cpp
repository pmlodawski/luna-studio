
#ifdef _WIN32
	#pragma comment(lib, "libprotobuf.lib")
	#define _WIN32_WINNT 0x501 //Needs to come before asio.hpp
#endif


#include <chrono>
#include <iostream>
#include <string>
#include <algorithm>
#include <type_traits>
#include <cstdint>

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

Response callAndTranslateException(Socket &socket, const Request& request)
{
	auto response = call(socket, request);

	if(response.type() == generated::proto::batch::Response_Type_Exception)
	{
		const auto exc = response.GetExtension(Exception::rsp);
		const auto msg = exc.message();
		throw std::runtime_error(msg);
	}

	return response;
}

typedef std::vector<std::pair<generated::proto::crumb::Crumb_Cls, std::string>> BreadcrumbsHelper;
generated::proto::crumb::Breadcrumbs *buildBreadcrumbs(const BreadcrumbsHelper &breadcrumbsInfo)
{
	auto breadcrumbs = new generated::proto::crumb::Breadcrumbs();
	for(auto &crumbDescriptor : breadcrumbsInfo)
	{
		auto crumb = breadcrumbs->mutable_crumbs()->Add();
		crumb->set_cls(crumbDescriptor.first);
		crumb->set_name(crumbDescriptor.second);
	}
	return breadcrumbs;
}

template <typename TMessage>
void setField(TMessage *msg, int i, int value)
{
	 msg->GetReflection()->SetInt32(msg, TMessage::descriptor()->field(i), value);
}


template <typename TMessage>
void setField(TMessage *msg, int i, std::string value)
{
	 msg->GetReflection()->SetString(msg, TMessage::descriptor()->field(i), value);
}

template <typename TMessage, typename SomeMessageType>
void setField(TMessage *msg, int i, SomeMessageType *value)
{
	static_assert(std::is_base_of<google::protobuf::Message, SomeMessageType>::value, "Cannot set field to a non-message pointer");
	auto messageField = msg->GetReflection()->MutableMessage(msg, TMessage::descriptor()->field(i));
	messageField->GetReflection()->Swap(messageField, value);
	delete value;
}
/*
template <typename TMessage, typename TField>
void setField(TMessage *msg, int, const TField &)
{
	static_assert(0, "Don't know how to set this kind of value");
}*/

template <int index, typename TMessage>
void setArgsInternal(TMessage *ms)
{
}

template <int index, typename TMessage, typename Arg, typename ... Args>
void setArgsInternal(TMessage *msg, Arg arg1, Args ... args)
{
	setField(msg, index, arg1);
	setArgsInternal<index + 1>(msg, args...);
}

template <typename TMessage, typename ... Args>
TMessage *buildArgs(Args ... args)
{
	auto ret = new TMessage(); 
	setArgsInternal<0, TMessage, Args...>(ret, args...);
	return ret;
}

template<typename TResult, typename TArgs>
TResult askSecondaryWrapper(tcp::socket &socket, TArgs * args, 
							 generated::proto::batch::Request_Method method)
{
	Request request;
	request.set_method(method);
	request.SetAllocatedExtension(TArgs::req, args);

	auto response = callAndTranslateException(socket, request);
	assert(response.type() == Response_Type_Result); //exception would be translated to exception
	auto defaultsResponse = response.GetExtension(TResult::rsp);
	response.Clear();
	return defaultsResponse;
}

#define makeAsk(space, method) namespace macro { namespace space {  \
space ##_ ## method ## _Result                                      \
method(tcp::socket &socket, space ## _ ## method ## _Args *args)    \
{                                                                   \
	return askSecondaryWrapper                                      \
	<space ##_ ## method ## _Result, space ## _ ## method ## _Args> \
	(socket, args, Request_Method_ ## space ## _ ## method);        \
} } } 

makeAsk(NodeDefault, NodeDefaults)
makeAsk(NodeDefault, SetNodeDefault)
makeAsk(NodeDefault, RemoveNodeDefault)

//makeAsk(AST, AST_Definitions)
makeAsk(AST, AddModule)
makeAsk(AST, AddClass)
makeAsk(AST, AddFunction)
makeAsk(AST, UpdateModuleCls)
makeAsk(AST, UpdateModuleImports)
makeAsk(AST, UpdateModuleFields)
makeAsk(AST, UpdateClassCls)
makeAsk(AST, UpdateClassFields)
makeAsk(AST, UpdateFunctionName)
makeAsk(AST, UpdateFunctionPath)
makeAsk(AST, UpdateFunctionInputs)
makeAsk(AST, UpdateFunctionOutput)
makeAsk(AST, Remove)

makeAsk(FileSystem, LS)
makeAsk(FileSystem, Stat)
makeAsk(FileSystem, MkDir)
makeAsk(FileSystem, Touch)
makeAsk(FileSystem, RM)
makeAsk(FileSystem, CP)
makeAsk(FileSystem, MV)

makeAsk(Graph, NodesGraph)
makeAsk(Graph, NodeByID)
makeAsk(Graph, AddNode)
makeAsk(Graph, UpdateNode)
makeAsk(Graph, RemoveNode)
makeAsk(Graph, Connect)
makeAsk(Graph, Disconnect)

makeAsk(Maintenance, Initialize)
makeAsk(Maintenance, Ping)
makeAsk(Maintenance, Dump)
makeAsk(Maintenance, Shutdown)

makeAsk(Project, Projects)
makeAsk(Project, ProjectByID)
makeAsk(Project, CreateProject)
makeAsk(Project, OpenProject)
makeAsk(Project, UpdateProject)
makeAsk(Project, CloseProject)
makeAsk(Project, StoreProject)

makeAsk(Library, Libraries)
makeAsk(Library, LibraryByID)
makeAsk(Library, CreateLibrary)
makeAsk(Library, LoadLibrary)
makeAsk(Library, UnloadLibrary)
makeAsk(Library, StoreLibrary)
makeAsk(Library, RunLibrary)

NodeDefault_NodeDefaults_Result askForNodeDefaults(tcp::socket &socket, int pid, int lid, BreadcrumbsHelper breadcrumbsInfo, int nid)
{
	auto *bc = buildBreadcrumbs({ { crumb::Crumb_Cls_FunctionCrumb, "main" } });
	auto *args = buildArgs<NodeDefault_NodeDefaults_Args>(nid, bc, lid, pid);
	return macro::NodeDefault::NodeDefaults(socket, args);
}

void temporaryFunctionForExperiments(tcp::socket &socket)
{
//	auto result = macro::FileSystem::LS(socket, buildArgs<FileSystem_LS_Args>("~"));
}

void ping(tcp::socket &socket)
{
	macro::Maintenance::Ping(socket, buildArgs<Maintenance_Ping_Args>());
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
		const std::string host = "localhost", port = "30521";

		std::cout << "Connecting to " << host << " at port " << port << std::endl;
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

		try
		{
			using namespace generated::proto::crumb;
			auto defaults = askForNodeDefaults(socket, 0, 0, { { Crumb_Cls_FunctionCrumb, "main" } }, 0);
		}
		catch(std::exception &e)
		{
			std::cout << "Cannot get node defaults. Error: " << e.what() << std::endl;
		}
		std::cout << "Query for node defaults: " << sw.elapsedMs().count() << "ms\n";

		const int nodeDefaultsAskCount = 100;
		for(int i = 0; i < nodeDefaultsAskCount; i++)
		{
			try
			{
				using namespace generated::proto::crumb;
				auto defaults = askForNodeDefaults(socket, 0, 0, { { Crumb_Cls_FunctionCrumb, "main" } }, 0);
			}
			catch(std::exception &e)
			{
				std::cout << "Cannot get node defaults. Error: " << e.what() << std::endl;
			}
		}
		std::cout << nodeDefaultsAskCount << " queries for node defaults: " << sw.elapsedMs().count() << "ms\n";

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
