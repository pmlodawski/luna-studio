#include <fstream>

#include "generated/server-api.pb.h"
#include <boost/algorithm/string.hpp>

#ifdef _WIN32
#pragma comment(lib, "libprotobuf.lib")
#endif


using namespace generated::proto::batch;
using namespace google::protobuf;


void formatOutput(std::ostream &out, std::string contents)
{
	auto hlp = contents;
	boost::replace_all(hlp, "\t", "");
	int count = 0;
	std::string outtxt;
	for(int i = 0; i < hlp.size(); i++)
	{
		const char &c = hlp[i];

		if(c == '}'  &&  outtxt.back() == '\t') outtxt.pop_back();

		outtxt.push_back(c);
		if(c == '{') count++;
		else if(c == '}') count--;
		else if(c == '\n') for(int j = 0; j < count; j++) outtxt.push_back('\t');

	}

	out << outtxt;
}


const std::string headerFile = R"(
#pragma once

#include <string>
#include <vector>
#include <boost/optional.hpp>
#include <boost/asio.hpp>

#include "../BatchIdWrappers.h"


template <typename T>
struct AsyncBatchRequest;

template <typename T>
struct AsyncBatchRequest
{
	typedef boost::variant<T, BatchException> Variant;
	typedef Maybe<Variant> MaybeResp; //response is either T or exception
	SharedCondVariable<MaybeResp> response;

	std::function<void(Variant &)> callback;

public:
	void setResponse(const T &Response)
	{
		auto lock = boost::make_unique_lock(response.mx);
		response.data = Variant(Response);
		if(callback)
			callback(*response.data);
	}
	void setException(const BatchException &Response)
	{
		//TODO? unify with the function above?
		auto lock = boost::make_unique_lock(response.mx);
		response.data = Variant(Response);
		if(callback)
			callback(*response.data);
	}

	generated::proto::batch::Response getResponse() //syncs
	{
		response.waitUntil([](const MaybeResp & r) { return (bool)r; });
		return response.access([](const MaybeResp &r) 
		{
			auto &hlp = *r;
			if(auto e = boost::get<BatchException>(&hlp))
			{
				logWarning("Rethrowing batch exception %s", e);
				throw e;
			}
			else if(auto t = boost::get<T>(&hlp))
			{
				return t;
			}
			THROW("Broken request structure");
		});
	}


	void setCallback(const std::function<void(Variant&)> &Callback)
	{
		auto lock = boost::make_unique_lock(response.mx);
		if(response.data)
			Callback(*response.data);
		else
			callback = Callback;
	}
};


template <>
struct AsyncBatchRequest<void>
{
	typedef boost::variant<int, BatchException> Variant;
	typedef Maybe<Variant> MaybeResp; //response is either T or exception
	SharedCondVariable<MaybeResp> response;

	std::function<void(Variant &)> callback;

public:
	void setResponse()
	{
		auto lock = boost::make_unique_lock(response.mx);
		response.data = Variant(0);
		if(callback)
			callback(*response.data);
	}
	void setException(const BatchException &Response)
	{
		//TODO? unify with the function above?
		auto lock = boost::make_unique_lock(response.mx);
		response.data = Variant(Response);
		if(callback)
			callback(*response.data);
	}

	void getResponse() //syncs
	{
		response.waitUntil([](const MaybeResp & r) { return (bool)r; });
		return response.access([](const MaybeResp &r) 
		{
			auto &hlp = *r;
			if(auto e = boost::get<BatchException>(&hlp))
			{
				logWarning("Rethrowing batch exception %s", e);
				throw e;
			}

			return;
		});
	}


	void setCallback(const std::function<void(Variant &)> &Callback)
	{
		auto lock = boost::make_unique_lock(response.mx);
		if(response.data)
			Callback(*response.data);
		else
			callback = Callback;
	}
};

struct AsynchronousCallbacks
{
	typedef std::function<void(std::unique_ptr<generated::proto::batch::Response>)> TCallback;
	std::map<int, TCallback> unmatchedCallbacks;
	std::map<int, std::unique_ptr<generated::proto::batch::Response>> unmatchedRespones;

	void addCallback(int id, const TCallback &cb);
	void addResponse(std::unique_ptr<generated::proto::batch::Response> response);
};

class %wrapper_name%
{
public:

static size_t sendAll(boost::asio::ip::tcp::socket &s, void *data, size_t size);
static void sendRequest(boost::asio::ip::tcp::socket &s, const generated::proto::batch::Request& request);
static std::unique_ptr<generated::proto::batch::Response> receiveResponse(boost::asio::ip::tcp::socket &s);
static std::unique_ptr<generated::proto::batch::Response> call(boost::asio::ip::tcp::socket &s, const generated::proto::batch::Request& request);
static std::unique_ptr<generated::proto::batch::Response> callAndTranslateException(boost::asio::ip::tcp::socket &s, const generated::proto::batch::Request& request);


std::function<void(const std::string&)> before;
std::function<void(const std::string&)> success;
std::function<void(const std::string&)> error;
std::function<void(const std::string&)> after;

std::function<generated::proto::crumb::Breadcrumbs(DefinitionId defID)> crumbifyMethod;


generated::proto::crumb::Breadcrumbs crumbify(DefinitionId defID) 
{
	try
	{
		if(!crumbifyMethod) 
			throw std::runtime_error("No crumbifyMethod provided!");
		
		return crumbifyMethod(defID);
	}
	catch(std::exception &e)
	{
		THROW("Failed to translate id %s to BreadCrumbs: %s.", defID, e);
	}
}

boost::asio::ip::tcp::socket &socket;
boost::asio::ip::tcp::socket &notifySocket;

SharedCondVariable<AsynchronousCallbacks> requestsInfo;
	
void handleNotificationSocket();

%wrapper_name%(boost::asio::ip::tcp::socket &socket, boost::asio::ip::tcp::socket &notifySocket) : socket(socket), notifySocket(notifySocket) {}
%method_decls%
};

%ext_to_enum%

)";

const std::string sourceFile = R"(

#include "BatchClient.h"

#include <google/protobuf/io/zero_copy_stream_impl.h>
#include <google/protobuf/io/coded_stream.h>


const int BUFFER_SIZE = 10000000; // TODO [PM] : magic constant

using boost::asio::ip::tcp;
using Socket = tcp::socket;

size_t %wrapper_name%::sendAll(boost::asio::ip::tcp::socket &s, void *data, size_t size)
{
	size_t sent = boost::asio::write(s, boost::asio::buffer(data, size));
	assert(sent == size);
	return sent;
}


void %wrapper_name%::sendRequest(boost::asio::ip::tcp::socket &s, const generated::proto::batch::Request& request)
{
	int requestSize = request.ByteSize() + 4;
	std::vector<char> requestBuf(requestSize, 0);

	//write varint delimiter to buffer
	google::protobuf::io::ArrayOutputStream arrayOut(requestBuf.data(), requestSize);
	google::protobuf::io::CodedOutputStream codedOut(&arrayOut);
	codedOut.WriteVarint32(request.ByteSize());

	//write protobuf ack to buffer
	request.SerializeToCodedStream(&codedOut);

	//send buffer to client
	sendAll(s, requestBuf.data(), requestSize);
}

std::unique_ptr<generated::proto::batch::Response> %wrapper_name%::receiveResponse(boost::asio::ip::tcp::socket &s)
{
	static char buffer[BUFFER_SIZE];
	
	std::unique_ptr<generated::proto::batch::Response> response = std::unique_ptr<generated::proto::batch::Response>(new generated::proto::batch::Response());
	size_t received = boost::asio::read(s, boost::asio::buffer(buffer, 4));
	
	//read varint delimited protobuf object in to buffer
	//there's no method to do this in the C++ library so here's the workaround
	google::protobuf::io::ArrayInputStream headerArrayIn(buffer, received);
	google::protobuf::io::CodedInputStream headerCodedIn(&headerArrayIn);
	google::protobuf::uint32 packetSize;
	headerCodedIn.ReadVarint32(&packetSize);
	const int sizeinfoLength = headerCodedIn.CurrentPosition();
	const int remainingToRead = packetSize + sizeinfoLength - received;
	
	received = boost::asio::read(s, boost::asio::buffer(buffer + received, remainingToRead));
	
	google::protobuf::io::ArrayInputStream arrayIn(buffer + sizeinfoLength, packetSize);
	google::protobuf::io::CodedInputStream codedIn(&arrayIn);
	google::protobuf::io::CodedInputStream::Limit msgLimit = codedIn.PushLimit(packetSize);
	response->ParseFromCodedStream(&codedIn);
	codedIn.PopLimit(msgLimit);
	return response;
}

std::unique_ptr<generated::proto::batch::Response> %wrapper_name%::call(boost::asio::ip::tcp::socket &s, const generated::proto::batch::Request& request)
{
	sendRequest(s, request);
	return receiveResponse(s);
}

std::unique_ptr<generated::proto::batch::Response> %wrapper_name%::callAndTranslateException(boost::asio::ip::tcp::socket &s, const generated::proto::batch::Request& request)
{
	auto response = call(s, request);

	if(response->type() == generated::proto::batch::Response_Type_Exception)
	{
		const auto exc = response->GetExtension(generated::proto::batch::Exception::rsp);
		const auto msg = exc.message();
		throw std::runtime_error(msg);
	}

	return response;
}

%method_impls%

void BatchClient::handleNotificationSocket()
{
	while(true)
	{
		try
		{
			requestsInfo.waitUntil([](const AsynchronousCallbacks &m)
			{
				return m.unmatchedCallbacks.size();
			});

			auto response = receiveResponse(notifySocket);
			if(!response->has_id())
				THROW("Received a response on notification socket that has no id!");

			logTrace("Notification socket got a reply for request %d", response->id());
			int id = response->id();
			requestsInfo.access([&](AsynchronousCallbacks &m)
			{
				m.addResponse(std::move(response));
			});
		}
		catch(std::exception &e)
		{
			logError("Encountered an exception when listening to notification socket: %s", e);
			if(!notifySocket.is_open())
			{
				logWarning("Notification socket has been closed, ending notification listener thread.");
				return;
			}
		}
	}
}

void AsynchronousCallbacks::addCallback(int id, const TCallback &cb)
{
	if(unmatchedRespones.count(id))
	{
		logTrace("Already got response for query %d", id);
		cb(std::move(unmatchedRespones.at(id)));
		unmatchedRespones.erase(id);
	}
	else
	{
		logTrace("Queuing callback for query %d", id);
		assert(!unmatchedCallbacks.count(id));
		unmatchedCallbacks[id] = cb;
	}
}

void AsynchronousCallbacks::addResponse(std::unique_ptr<generated::proto::batch::Response> response)
{
	int id = response->id();
	if(!unmatchedCallbacks.count(id))
	{
		logWarning("Received a response on notification socket with id=%d that cannot be paired with any unanswered request!", id);
		assert(!unmatchedRespones.count(id));
		unmatchedRespones[id] = std::move(response);
	}
	else
	{
		logTrace("Notifying callback for query %d", id);
		unmatchedCallbacks.at(id)(std::move(response));
		unmatchedCallbacks.erase(id);
	}
}

)";

const std::string methodDeclaration = "%rettype% %space%_%method%(%args_list%);";
const std::string methodDeclarationAsync = "shared_ptr<AsyncBatchRequest<%async_rettype%>> %space%_%method%_async(%args_list%);";

const std::string methodDefinition = R"(
%rettype% %wrapper_name%::%space%_%method%(%args_list%)
{
	if(!this) THROW("Attempt to call method %space%_%method% while the batch connection wasn't st up (call to nullptr)!");
	if(before) before("%space%_%method%");
	FINALIZE{ if(after) after("%space%_%method%"); };
	try
	{
		
		generated::proto::batch::%space%_%method%_Args *args = new generated::proto::batch::%space%_%method%_Args();
		%setters%

		generated::proto::batch::Request request;
		request.set_method(generated::proto::batch::Request_Method_%space%_%method%);
		request.SetAllocatedExtension(generated::proto::batch::%space%_%method%_Args::req, args);

		std::unique_ptr<generated::proto::batch::Response> response;
		{
			StopWatch sw;
			FINALIZE{ logTrace("Batch call %s took %d ms.", "%space%_%method%", sw.elapsedMs().count()); };
			response = callAndTranslateException(socket, request);
		}
		assert(response->type() == generated::proto::batch::Response_Type_Result); //exception would be translated to exception

		FINALIZE{ if(success) success("%space%_%method%"); };
		%epilogue%
	}
	catch(std::exception &e)
	{
		if(error) error("%space%_%method%");
		//std::string msg = std::string("Call to batch method %space%::%method% triggered an exception: ") + e.what();
		throw BatchException(e.what(), "%method%");
	}
}

shared_ptr<AsyncBatchRequest<%async_rettype%>> %wrapper_name%::%space%_%method%_async(%args_list%)
{
	if(!this) THROW("Attempt to call method %space%_%method%_async while the batch connection wasn't st up (call to nullptr)!");
	if(before) before("%space%_%method%_async");
	FINALIZE{ if(after) after("%space%_%method%_async"); };
	try
	{
		
		generated::proto::batch::%space%_%method%_Args *args = new generated::proto::batch::%space%_%method%_Args();
		%setters%

		generated::proto::batch::Request request;
		request.set_async(true);
		request.set_method(generated::proto::batch::Request_Method_%space%_%method%);
		request.SetAllocatedExtension(generated::proto::batch::%space%_%method%_Args::req, args);

		std::unique_ptr<generated::proto::batch::Response> response;
		{
			StopWatch sw;
			FINALIZE{ logTrace("Batch call %s tool %d ms (just making the request).", "%space%_%method%_async", sw.elapsedMs().count()); };
			response = callAndTranslateException(socket, request);
		}
		assert(response->type() == generated::proto::batch::Response_Type_Accept); //exception would be translated to exception
		assert(response->has_id());
		const int requestID = response->id();

		FINALIZE{ if(success) success("%space%_%method%_async"); };
		auto retAsync = make_shared<AsyncBatchRequest<%async_rettype%>>();

		auto onResponseReceived = [=](std::unique_ptr<generated::proto::batch::Response> response)
		{
			if(response->type() == generated::proto::batch::Response_Type_Exception)
			{
				const auto exc = response->GetExtension(generated::proto::batch::Exception::rsp);
				BatchException e = BatchException(exc.message(), "%space%_%method%_async");
				retAsync->setException(e);
			}
			else
			{
				%prepare_ret%
				retAsync->setResponse(%async_use_ret%);
			}
		};


		requestsInfo.access([=](AsynchronousCallbacks &m)
		{
			m.addCallback(requestID, onResponseReceived);
		});

		return retAsync;
	}
	catch(std::exception &e)
	{
		if(error) error("%space%_%method%");
		//std::string msg = std::string("Call to batch method %space%::%method% triggered an exception: ") + e.what();
		throw BatchException(e.what(), "%method%");
	}
}

)";

const std::string clsGetterMethod = R"(
inline generated::proto::%fname%::%msg%_Cls getCls(const generated::proto::%fname%::%ext% *arg)
{
	return generated::proto::%fname%::%msg%_Cls_%ext%;
}
)";

struct ArgWrapper
{
	const FieldDescriptor *arg;

	ArgWrapper(const FieldDescriptor*arg) :arg(arg) {}

	std::string translateBaseType(bool stripRef = false) const
	{
		FieldDescriptor::Type type = arg->type();
		switch(type)
		{
		case FieldDescriptor::TYPE_DOUBLE:
			return "double";
		case FieldDescriptor::TYPE_BOOL:
			return "bool";
		case FieldDescriptor::TYPE_INT32:
			return "int";
		case FieldDescriptor::TYPE_STRING:
		case FieldDescriptor::TYPE_BYTES:
			return stripRef ? "std::string" : "const std::string &";
		case FieldDescriptor::TYPE_MESSAGE:
		{
			auto ret = arg->message_type()->full_name();
			boost::replace_all(ret, ".", "::");
			return ret;
		}
		default:
			assert(0);
		}

		assert(0);
		return {};
	}

	std::string translateType(bool asValue = false) const
	{
		std::string ret;
		if(arg->is_repeated())
		{
			if(asValue)
				ret = "std::vector<%2>";
			else
				ret = "const std::vector<%2> &";
		}
		else if(arg->is_optional())
		{
			if(asValue)
				ret = "boost::optional<%2>";
			else
				ret = "const boost::optional<%2> &";
		}
		else if(arg->type() == FieldDescriptor::TYPE_MESSAGE)
		{
			if(asValue)
				ret = "%1";
			else
				ret = "const %1 &";
		}
		else
			ret = "%1";

		boost::replace_all(ret, "%1", translateBaseType(false));
		boost::replace_all(ret, "%2", translateBaseType(true));
		return ret;
	}

	std::string formatArgument() const
	{
		return translateType() + " " + arg->name();
	}
};

struct MethodWrapper
{
	std::vector<const FieldDescriptor*> argsFields;
	const Descriptor *args, *result;
	const EnumValueDescriptor *methodValue;

	std::string space, name;

	std::string returnedType, returnedTypeAsync, epilogue, arguments, setters;
	std::string prepareRet, returnRet, useRet, useRetAsync;

	std::vector<int> collapsedArgs;
	std::string collapsedName; 

	MethodWrapper(const FileDescriptor *file, const EnumValueDescriptor *method) : methodValue(method)
	{
		std::vector<std::string> parts;
		boost::split(parts, methodValue->name(), boost::is_any_of("_"));
		assert(parts.size() == 2);
		space = parts[0];
		name = parts[1];

		auto spaceDescriptor = file->FindMessageTypeByName(parts[0]);
		auto methodDescriptor = spaceDescriptor->FindNestedTypeByName(parts[1]);

		args = methodDescriptor->FindNestedTypeByName("Args");
		result = methodDescriptor->FindNestedTypeByName("Result");
		for(int i = 0; i < args->field_count(); i++)
			argsFields.push_back(args->field(i));

		arguments = translateArguments();
		setters = formatSetters();
		formatEpilogue();
	}

	std::string format(const std::string &input) const
	{
		auto ret = input;
		boost::replace_all(ret, "%space%", space);
		boost::replace_all(ret, "%method%", name);
		boost::replace_all(ret, "%args_list%", arguments);
		boost::replace_all(ret, "%setters%", setters);
		boost::replace_all(ret, "%epilogue%", epilogue);
		boost::replace_all(ret, "%rettype%", returnedType);
		boost::replace_all(ret, "%async_rettype%", returnedTypeAsync);
		boost::replace_all(ret, "%prepare_ret%", prepareRet);
		boost::replace_all(ret, "%use_ret%", useRet);
		boost::replace_all(ret, "%async_use_ret%", useRetAsync);

		//boost::replace_all(ret, "%rettype%", returnedType);
		return ret;
	}

	std::string formatDecl() const
	{
		return format(methodDeclaration) + "\n" + format(methodDeclarationAsync);
	}

	std::string formatImpl() const
	{
		return format(methodDefinition);
	}

	std::string translateArguments() 
	{
		std::vector<std::string> argsTxt;
		for(int i = 0; i < argsFields.size(); i++)
		{
			auto &arg = argsFields.at(i);
			if(arg->name() == "nodeID" && argsFields.at(i + 1)->name() == "bc")
			{
				assert(argsFields.at(i + 1)->name() == "bc");
				assert(argsFields.at(i + 2)->name() == "libraryID");
				assert(argsFields.at(i + 3)->name() == "projectID");
				collapsedArgs.resize(4, i);
				i += 3;
				argsTxt.push_back("const NodeId &nodeID");
				collapsedName = "nodeID";
			}
			else if(arg->name() == "bc" || arg->name() == "parentbc")
			{
				assert(argsFields.at(i + 1)->name() == "libraryID");
				assert(argsFields.at(i + 2)->name() == "projectID");
				collapsedArgs.resize(3, i);
				i += 2;

				if(arg->name() == "bc")
					collapsedName = "defID";
				else
					collapsedName = "parent";
				argsTxt.push_back("const DefinitionId &" + collapsedName);
			}
			else if(arg->name() == "libraryID")
			{
				assert(argsFields.at(i + 1)->name() == "projectID");
				collapsedArgs.resize(2, i);
				i += 1;

				collapsedName = "libID";
				argsTxt.push_back("const LibraryId &libID");
			}
			else if(arg->name() == "projectID")
			{
				collapsedArgs.resize(1, i);
				collapsedName = "projID";
				argsTxt.push_back("const ProjectId &projID");
			}
			else
			{
				argsTxt.push_back(ArgWrapper(arg).formatArgument());
			}
		}

		return boost::join(argsTxt, ", ");
	}

	std::string formatSetters() const
	{
		std::string ret;
		for(int i = 0; i < argsFields.size(); i++)
		{
			auto &arg = argsFields.at(i);
			if(collapsedArgs.size() && collapsedArgs.front() == i)
			{
				for(int j = 0; j < collapsedArgs.size(); j++)
				{
					auto &argInner = argsFields.at(i+j);

					static const std::string names[] = { "nodeID", "defID", "libID", "projID" };
					int index = 4-collapsedArgs.size()+j;
					auto derefedArg = collapsedName + "." + names[index];
					if(index == 1)
					{
						ret += "args->mutable_" + argInner->lowercase_name() + "()->CopyFrom(crumbify(" + collapsedName + "));\n";
					}
					else
						ret += "args->set_" + argInner->lowercase_name() + "(" + derefedArg + ");\n";
				}
				i += collapsedArgs.size() - 1;
			}
			else
			{
				std::string entry;
				if(arg->is_repeated())
				{
					if(arg->type() == FieldDescriptor::TYPE_MESSAGE)
					{
						entry =
							R"(for(size_t i = 0; i < %1.size(); i++)
								{
									auto added = args->add_%2();
									added->MergeFrom(%1[i]);
								})";
					}
					else
					{
						entry =
							R"(	for(size_t i = 0; i < %1.size(); i++)
								{
									assert(args->%2_size() == i);
									args->add_%2(%1.at(i));
								})";
					}
				}
				else if(arg->is_optional())
					entry = "if(%1)\n{\nargs->set_%2(*%1);\n}";
				else if(arg->type() == FieldDescriptor::TYPE_MESSAGE)
					entry = "args->mutable_%2()->CopyFrom(%1);";
				else
					entry = "args->set_%2(%1);";

				boost::replace_all(entry, "%1", arg->name());
				boost::replace_all(entry, "%2", arg->lowercase_name());
				ret += entry + "\n";
			}
		}
		return ret;
	}

	void formatEpilogue()
	{
		auto resultPack = "generated::proto::batch::" + space + "_" + name + "_Result";
		if(result->field_count() == 0)
		{
			returnedType = "void";
			prepareRet = "";
			returnRet = "return;";
			useRet = "";
		}
		else if(result->field_count() == 1)
		{
			auto field = result->field(0);
			ArgWrapper arg(field);

			returnedType = arg.translateType(true);
			if(!field->is_repeated()  &&  !field->is_optional()  &&  field->type() == FieldDescriptor::TYPE_MESSAGE)
			{
				returnedTypeAsync = returnedType;
				useRetAsync = "*ret";
				returnedType = "std::unique_ptr<" + returnedType + ">";
			}

			prepareRet = resultPack + " & result = *response->MutableExtension(" + resultPack + "::rsp);\n";
			
			if(field->is_repeated())
			{
				prepareRet += returnedType + " ret;\n";
				prepareRet += "for(int i = 0; i < result."+field->lowercase_name()+"_size(); i++)\n{\n";
				prepareRet += "ret.push_back(result."+field->lowercase_name()+"(i));\n}\n";
			}
			else if(field->is_optional())
			{
				prepareRet += returnedType + " ret;\n";
				prepareRet += "if(result.has_" + field->lowercase_name() + "()) \n{\n";
				prepareRet += returnedType + " ret = result." + field->lowercase_name() + "();\n}\n";
			}
			else if(field->type() == FieldDescriptor::TYPE_MESSAGE)
			{
				prepareRet += "auto retHlp = result.release_" + field->lowercase_name() + "();\n";
				prepareRet += returnedType + " ret = " + returnedType + "(retHlp);\n";
			}
			else
			{
				prepareRet += returnedType + " ret = result." + field->lowercase_name() + "();\n";
			}
			returnRet = "return ret;";
			useRet = "ret";
		}
		else
		{
			returnedType = "generated::proto::batch::" + space + "_" + name + "_Result";
			prepareRet = returnedType + " ret;\n";
			prepareRet += "ret = response->GetExtension(" + returnedType + "::rsp);";
			returnRet = "return ret;";
			useRet = "ret";
		}

		if(returnedTypeAsync.empty())
			returnedTypeAsync = returnedType;
		if(useRetAsync.empty())
			useRetAsync = useRet;

		epilogue = prepareRet + "\n" + returnRet;
	}
};

//methods covnerting between expr/pat/type -> cls 
std::string extToClsCovnersions()
{
	std::string ret;

	std::vector<std::string> toHandle = { "Expr", "Pat", "Type" };
	auto d = AST::descriptor();
	auto f = d->file();
	for(int i = 0; i < f->dependency_count(); i++)
	{
		auto dep = f->dependency(i);
		for(int j = 0; j < dep->message_type_count(); j++)
		{
			auto msg = dep->message_type(j);
			auto e = msg->FindEnumTypeByName("Cls");
			//std::cout << msg->full_name() << std::endl;
			if(e  &&  std::find(toHandle.begin(), toHandle.end(), e->containing_type()->name()) != toHandle.end())
			{
				auto fname = e->file()->name();
				boost::replace_last(fname, ".proto", "");

				std::cout << "\t" << e->containing_type()->name() << " -> " << e->full_name() << std::endl;
				for(int k = 0; k < e->value_count(); k++)
				{
					auto enumVal = e->value(k);

					auto hlp = clsGetterMethod;

					boost::replace_all(hlp, "%fname%", fname);
					boost::replace_all(hlp, "%msg%", msg->name());
					boost::replace_all(hlp, "%ext%", enumVal->name());
					ret += hlp;
				}
				//boost::replace_all(hlp, "%ext_msg%", )
				//std::cout << hlp << std::endl;
			}
		}

		//std::cout << dep->name() << std::endl;
	}
	return ret;
}

void generate(const std::string &outputFile)
{
	std::string methodImpls;
	std::string methodDecls;

	auto fileDescriptor = AST::descriptor()->file();
	auto methodsDescriptor = Request::Method_descriptor();
	for(int i = 0; i < methodsDescriptor->value_count(); i++)
	{
		methodImpls += MethodWrapper(fileDescriptor, methodsDescriptor->value(i)).formatImpl();
		methodDecls += MethodWrapper(fileDescriptor, methodsDescriptor->value(i)).formatDecl() + "\n";
	}

	auto formatFile = [&](const std::string &input) -> std::string
	{
		auto ret = input;
		boost::replace_all(ret, "%method_decls%", methodDecls);
		boost::replace_all(ret, "%method_impls%", methodImpls);
		boost::replace_all(ret, "%wrapper_name%", "BatchClient");
		boost::replace_all(ret, "%ext_to_enum%", extToClsCovnersions());
		return ret;
	};

	{
		std::ofstream out(outputFile + ".cpp");
		formatOutput(out, formatFile(sourceFile));
	}
	{
		std::ofstream out(outputFile + ".h");
		formatOutput(out, formatFile(headerFile));
	}

	for(int i = 0; i < fileDescriptor->message_type_count(); i++)
	{
		auto messageDescriptor = fileDescriptor->message_type(i);
		int g = 4;
	}
}

int main()
{
	extToClsCovnersions();
	generate("generated/BatchClient");
	return EXIT_SUCCESS;
}