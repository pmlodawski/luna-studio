
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

#include "generated/server-api.pb.h"  
#include "generated/attributes.pb.h"  

#include <boost/asio.hpp>



using boost::asio::ip::tcp;
using Socket = tcp::socket;

using namespace google::protobuf::io;

using namespace generated::proto;
using namespace generated::proto::batch;

const int BUFFER_SIZE = 10000000; // TODO [PM] : magic constant
char buffer[BUFFER_SIZE];

const std::string host = "localhost", controlPort = "30521", notifyPort = "30522";

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

size_t sendAll(Socket &controlSocket, void *data, size_t size)
{
	size_t sent = boost::asio::write(controlSocket, boost::asio::buffer(data, size));
	assert(sent == size);
	return sent;
}


void sendRequest(Socket &controlSocket, const Request& request)
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
	sendAll(controlSocket, requestBuf, requestSize);
	// std::cout << "Sent: " << sent << std::flush;

	delete [] requestBuf;
}

Response receiveResponse(Socket &controlSocket)
{
	Response response;
	size_t received = boost::asio::read(controlSocket, boost::asio::buffer(buffer, 3));

	//read varint delimited protobuf object in to buffer
	//there's no method to do this in the C++ library so here's the workaround
	ArrayInputStream headerArrayIn(buffer, received);
	CodedInputStream headerCodedIn(&headerArrayIn);
	google::protobuf::uint32 packetSize;
	headerCodedIn.ReadVarint32(&packetSize);
	const int sizeinfoLength = headerCodedIn.CurrentPosition();
	const int remainingToRead = packetSize + sizeinfoLength - received;
	received = boost::asio::read(controlSocket, boost::asio::buffer(buffer + received, remainingToRead));

	ArrayInputStream arrayIn(buffer + sizeinfoLength, packetSize);
	CodedInputStream codedIn(&arrayIn);
	CodedInputStream::Limit msgLimit = codedIn.PushLimit(packetSize);
	response.ParseFromCodedStream(&codedIn);
	codedIn.PopLimit(msgLimit);
	return response;
}

Response call(Socket &controlSocket, const Request& request)
{
	sendRequest(controlSocket, request);
	return receiveResponse(controlSocket);
}

Response callAndTranslateException(Socket &controlSocket, const Request& request)
{
	auto response = call(controlSocket, request);

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
void setField(TMessage *msg, int i, const char* value)
{
	msg->GetReflection()->SetString(msg, TMessage::descriptor()->field(i), value);
}

template <typename TMessage>
void setField(TMessage *msg, int i, std::string value)
{
	 msg->GetReflection()->SetString(msg, TMessage::descriptor()->field(i), value);
}

template <typename TMessage>
void setField(TMessage *msg, int i, std::vector<int> value)
{
	for(int index = 0 ; index < value.size() ; ++index)
		msg->GetReflection()->AddInt32(msg, TMessage::descriptor()->field(i), value[index]);
}

template <typename TMessage, typename SomeMessageType>
void setField(TMessage *msg, int i, SomeMessageType *value)
{
	static_assert(std::is_base_of<google::protobuf::Message, SomeMessageType>::value, "Cannot set field to a non-message pointer");
	auto messageField = msg->GetReflection()->MutableMessage(msg, TMessage::descriptor()->field(i));
	messageField->GetReflection()->Swap(messageField, value);
	delete value;
}

template <typename TMessage, typename SomeMessageType>
void setField(TMessage *msg, int i, SomeMessageType value)
{
	static_assert(std::is_base_of<google::protobuf::Message, SomeMessageType>::value, "Cannot set field to a non-message pointer");
	auto messageField = msg->GetReflection()->MutableMessage(msg, TMessage::descriptor()->field(i));
	messageField->GetReflection()->Swap(messageField, &value);
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
TResult askSecondaryWrapper(tcp::socket &controlSocket, TArgs * args, 
							 generated::proto::batch::Request_Method method)
{
	Request request;
	request.set_method(method);
	request.SetAllocatedExtension(TArgs::req, args);

	auto response = callAndTranslateException(controlSocket, request);
	assert(response.type() == Response_Type_Result); //exception would be translated to exception
	auto defaultsResponse = response.GetExtension(TResult::rsp);
	response.Clear();
	return defaultsResponse;
}

#define makeAsk(space, method) namespace macro { namespace space {              \
space ##_ ## method ## _Result                                                  \
method##_(tcp::socket &controlSocket, space ## _ ## method ## _Args *args)             \
{                                                                               \
	return askSecondaryWrapper                                                  \
	<space ##_ ## method ## _Result, space ## _ ## method ## _Args>             \
	(controlSocket, args, Request_Method_ ## space ## _ ## method);                    \
}                                                                               \
	                                                                            \
template<typename ...Args> 	                       	                            \
space ##_ ## method ## _Result                                                  \
method(tcp::socket &controlSocket, const Args & ...args)                               \
{                                                                               \
	return method##_(controlSocket, buildArgs<space ## _ ## method ## _Args>(args...));\
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
makeAsk(AST, UpdateDataCls)
makeAsk(AST, UpdateDataCons)
makeAsk(AST, UpdateDataClasses)
makeAsk(AST, UpdateDataMethods)
makeAsk(AST, UpdateFunctionName)
makeAsk(AST, UpdateFunctionPath)
makeAsk(AST, UpdateFunctionInputs)
makeAsk(AST, UpdateFunctionOutput)
makeAsk(AST, Remove)
makeAsk(AST, ResolveDefinition)

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
makeAsk(Library, BuildLibrary)



int main()
{
	try
	{
		//Prepare Boost.Asio
		boost::asio::io_service io_service;
		tcp::socket controlSocket(io_service);
		tcp::resolver resolver(io_service);

		tcp::socket notifySocket(io_service);


		//Connect
		std::cout << "Connecting to " << host << " at port " << controlPort << " / " << notifyPort << std::endl;
		boost::asio::connect(controlSocket, resolver.resolve({ host, controlPort }));
		boost::asio::connect(notifySocket, resolver.resolve({ host, notifyPort }));

		StopWatch sw;

		macro::Maintenance::Initialize(controlSocket);
		std::cout << sw.elapsedMs().count() << "ms\tMaintenance::Initialize\n";

		auto lsResult = macro::FileSystem::LS(controlSocket, "~");
		std::cout << sw.elapsedMs().count() << "ms\tFileSystem::LS\n";

		auto project = macro::Project::CreateProject(controlSocket, "testProject", "/tmp/flowbox/testProject", attributes::Attributes()).project();
		auto library = macro::Library::CreateLibrary(controlSocket, "MyLib", "/tmp/flowbox/testProject/testLibrary", project.id()).library();
		
		auto projects = macro::Project::Projects(controlSocket).projects();
		std::cout << "Projects loaded: " << projects.size() << std::endl;
		auto project2 = macro::Project::ProjectByID(controlSocket, project.id()).project();
		std::cout << "Project name: " << project2.name() << std::endl;

		auto libraries = macro::Library::Libraries(controlSocket, project.id()).libraries();
		std::cout << "Libraries loaded: " << libraries.size() << std::endl;
		auto library2 = macro::Library::LibraryByID(controlSocket, library.id(), project.id()).library();
		std::cout << "Library name: " << library2.name() << std::endl;

		const BreadcrumbsHelper crumbsRoot = { {crumb::Crumb_Cls_ModuleCrumb, "MyLib"} };
		const BreadcrumbsHelper crumbsMain = { {crumb::Crumb_Cls_ModuleCrumb, "MyLib"} 
											 , {crumb::Crumb_Cls_ModuleCrumb, "Main" } };
		const BreadcrumbsHelper crumbsTest = { {crumb::Crumb_Cls_ModuleCrumb, "MyLib"}
											 , {crumb::Crumb_Cls_ModuleCrumb, "Main" }
											 , {crumb::Crumb_Cls_FunctionCrumb, "test"} };
		{
			module::Module module;
			auto moduleTypeBase = module.mutable_type();
			moduleTypeBase->set_cls(type::Type::Module);
			auto moduleType = new type::Module();
			moduleType->add_path("Main");
			moduleTypeBase->SetAllocatedExtension(type::Module::ext, moduleType);
			auto bc_Root = buildBreadcrumbs(crumbsRoot);
			macro::AST::AddModule(controlSocket, module, bc_Root, library.id(), project.id());
		}
		{
			auto function = new expr::Function();
			function->set_name("test");
			auto unknown = new type::Type;
			unknown->set_cls(type::Type::Unknown);
			unknown->SetAllocatedExtension(type::Unknown::ext, new type::Unknown);
			function->set_allocated_output(unknown);
			expr::Expr functionBase;
			functionBase.set_cls(expr::Expr::Function);
			functionBase.SetAllocatedExtension(expr::Function::ext, function);
			auto bc_Main = buildBreadcrumbs(crumbsMain);
			macro::AST::AddFunction(controlSocket, functionBase, bc_Main, library.id(), project.id());
		}
		// macro::Maintenance::Dump(controlSocket);
		{
			auto type = new type::Data;
			type->set_name("A");
			auto typeBase = new type::Type;
			typeBase->set_cls(type::Type::Data);
			typeBase->SetAllocatedExtension(type::Data::ext, type);
			auto data = new expr::Data;
			data->set_allocated_cls(typeBase);
			data->mutable_cons();
			expr::Expr dataBase;
			dataBase.set_cls(expr::Expr::Data);
			dataBase.SetAllocatedExtension(expr::Data::ext, data);
			auto bc_Main = buildBreadcrumbs(crumbsMain);
			macro::AST::AddClass(controlSocket, dataBase, bc_Main, library.id(), project.id());
		}
		// macro::Maintenance::Dump(controlSocket);
		{
			auto bc_Main_test = buildBreadcrumbs(crumbsTest);
			macro::Graph::NodesGraph(controlSocket, bc_Main_test, library.id(), project.id());
		}
		int node45id;
		{
			auto bc_Main_test = buildBreadcrumbs(crumbsTest);
			graph::Node node;
			node.set_cls(graph::Node::Expr);
			node.set_expr("45"); 
			node.set_outputname("v45"); 
			node45id = macro::Graph::AddNode(controlSocket, node, bc_Main_test, library.id(), project.id()).nodeid();
		}
		int node90id;
		{
			auto bc_Main_test = buildBreadcrumbs(crumbsTest);
			graph::Node node;
			node.set_cls(graph::Node::Expr);
			node.set_expr("90"); 
			node90id = macro::Graph::AddNode(controlSocket, node, bc_Main_test, library.id(), project.id()).nodeid();
		}
		int nodeAddid;
		{
			auto bc_Main_test = buildBreadcrumbs(crumbsTest);
			graph::Node node;
			node.set_cls(graph::Node::Expr);
			node.set_expr("add"); 
			nodeAddid = macro::Graph::AddNode(controlSocket, node, bc_Main_test, library.id(), project.id()).nodeid();
		}
		int nodePrintid;
		{
			auto bc_Main_test = buildBreadcrumbs(crumbsTest);
			graph::Node node;
			node.set_cls(graph::Node::Expr);
			node.set_expr("print"); 
			nodePrintid = macro::Graph::AddNode(controlSocket, node, bc_Main_test, library.id(), project.id()).nodeid();
		}
		{
			auto bc_Main_test = buildBreadcrumbs(crumbsTest);
			macro::NodeDefault::SetNodeDefault(controlSocket, std::vector<int>{1}, "477", nodePrintid, bc_Main_test, library.id(), project.id());
		}
		{
			auto bc_Main_test = buildBreadcrumbs(crumbsTest);
			macro::NodeDefault::SetNodeDefault(controlSocket, std::vector<int>{0}, "Console", nodePrintid, bc_Main_test, library.id(), project.id());
		}
		macro::Maintenance::Dump(controlSocket);
		{
			auto bc_Main_test = buildBreadcrumbs(crumbsTest);
			macro::Graph::Connect(controlSocket, node90id, std::vector<int>{}, nodeAddid, std::vector<int>{0}, bc_Main_test, library.id(), project.id());
		}
		{
			auto bc_Main_test = buildBreadcrumbs(crumbsTest);
			macro::Graph::Connect(controlSocket, nodeAddid, std::vector<int>{}, -5, std::vector<int>{0}, bc_Main_test, library.id(), project.id());
		}
		{
			auto bc_Main_test = buildBreadcrumbs(crumbsTest);
			macro::Graph::Connect(controlSocket, node45id, std::vector<int>{}, nodeAddid, std::vector<int>{1}, bc_Main_test, library.id(), project.id());
		}
		{
			auto bc_Main_test = buildBreadcrumbs(crumbsMain);
			auto r = macro::AST::ResolveDefinition(controlSocket, "A", bc_Main_test, library.id(), project.id()).astptr();
			std::cout << "Result size: " << r.size() << std::endl;
		}
		macro::Maintenance::Dump(controlSocket);

		// macro::Library::BuildLibrary(controlSocket, library.id(), project.id());
		macro::Library::StoreLibrary(controlSocket, library.id(), project.id());
		macro::Library::UnloadLibrary(controlSocket, library.id(), project.id());
		macro::Project::StoreProject(controlSocket, project.id());
		macro::Project::CloseProject(controlSocket, project.id());

		auto loadedProject = macro::Project::OpenProject(controlSocket, "/tmp/flowbox/testProject").project();
		macro::Library::LoadLibrary(controlSocket, "/tmp/flowbox/testProject/testLibrary", loadedProject.id());

		// macro::Maintenance::Dump(controlSocket);
		macro::Project::CloseProject(controlSocket, loadedProject.id());

		// //////////////////////////////////////////////////////////////////////////
		// //////////////////////////////////////////////////////////////////////////
		// //////////////////////////////////////////////////////////////////////////
		// // Pings
		// const int pingCount = 10;
		// for(int i = 0; i < pingCount; ++i) 
		// 	macro::Maintenance::Ping(controlSocket);
		// std::cout << sw.elapsedMs().count() << "ms\t" << pingCount << " pings\n";

		// const int dataSize = 100000000;
		// {
		// 	Maintenance_Ping_Args* args = new Maintenance_Ping_Args();
		// 	std::vector<char> data(dataSize + 1, 65);
		// 	data.back() = 0;
		// 	args->set_data(data.data());

		// 	auto response = macro::Maintenance::Ping_(controlSocket, args);
		// }
		// std::cout << sw.elapsedMs().count() << "ms\tSending " << dataSize << " bytes\n";

		macro::Maintenance::Shutdown(controlSocket);
		std::cout << "done" << std::endl;

		return EXIT_SUCCESS;
	}
	catch(std::exception &e)
	{
		std::cout << "Encountered an exception: " << e.what() << std::endl;
		return EXIT_FAILURE;
	}
}
