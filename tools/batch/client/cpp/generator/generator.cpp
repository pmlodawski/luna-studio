
#include "../generated/project-manager.pb.h"
#include "../generated/file-manager.pb.h"
#include "../generated/parser.pb.h"
#include "../generated/plugin-manager.pb.h"

#include <boost/algorithm/string.hpp>
#include <boost/filesystem.hpp>
#include <boost/filesystem/fstream.hpp>

#ifdef _WIN32
#pragma comment(lib, "libprotobuf.lib")
#endif


using namespace boost::filesystem;
using namespace generated::proto::type;
using namespace google::protobuf;

const path outputDirectory = path("..") / "generated";

void formatOutput(std::ostream &out, std::string contents)
{
	auto hlp = contents;
	boost::replace_all(hlp, "\t", "");
	int count = 0;
	std::string outtxt;
	for(int i = 0; i < (int)hlp.size(); i++)
	{
		const char &c = hlp[i];

		if(c == '}'  &&  outtxt.back() == '\t') outtxt.pop_back();

		outtxt.push_back(c);
		if(c == '{') count++;
		else if(c == '}') count--;
		else if(c == '\n') for(int j = 0; j < count; j++) outtxt.push_back('\t');

	}

	out << outtxt << std::endl;
}


const std::string headerFile = R"(
#pragma once


#include "../BatchIdWrappers.h"
#include "../bus/BusLibrary.h"

class BusHandler;

class %wrapper_name%
{
public:
	shared_ptr<BusHandler> bh;

	CorrelationId sendRequest(std::string baseTopic, std::string requestTopic, const google::protobuf::Message &msg, ConversationDoneCb callback);

	std::function<void(const std::string&)> before;
	std::function<void(const std::string&)> success;
	std::function<void(const std::string&)> error;
	std::function<void(const std::string&)> after;

	std::function<generated::proto::crumb::Breadcrumbs(DefinitionId defID)> crumbifyMethod;


	generated::proto::crumb::Breadcrumbs crumbify(DefinitionId defID);

	%wrapper_name%(shared_ptr<BusHandler> bh) : bh(bh) {}
	%method_decls%
};

%ext_to_enum%

)";

const std::string sourceFile = R"(
#include "stdafx.h"
#include "%wrapper_name%.h"

#include <google/protobuf/io/zero_copy_stream_impl.h>
#include <google/protobuf/io/coded_stream.h>


CorrelationId %wrapper_name%::sendRequest(std::string baseTopic, std::string requestTopic, const google::protobuf::Message &msg, ConversationDoneCb callback)
{
	return bh->request(std::move(baseTopic), std::move(requestTopic), msg.SerializeAsString(), callback);
}

generated::proto::crumb::Breadcrumbs %wrapper_name%::crumbify(DefinitionId defID)
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

%method_impls%

)";

const std::string methodDeclaration = "%rettype% %method%(%args_list%);";
const std::string methodDeclarationAsync = "CorrelationId %method%_Async(%args_list_comma% ConversationDoneCb callback);";

const std::string methodDefinition = R"(
%rettype% %wrapper_name%::%method%(%args_list%)
{
	LOG_TIME("%topic% -- request in total");
	std::string topic = "%topic%";

#define USES_%answerType%

#ifdef USES_UPDATE
	std::string topicAnswer = "%topic%.update";
	typedef %namespace%::%method%_Update AnswerType;
#else
	std::string topicAnswer = "%topic%.status";
	typedef %namespace%::%method%_Status AnswerType;
#endif

	%define_ret%

	auto retrieveAnswer = [](const BusMessage &bm)
	{
		auto ret = make_unique<AnswerType>();
		ret->ParseFromString(bm.contents);
		return ret;
	};

	CondSh<bool> isDone;
	std::string errorMessage;

	// Because we are synchronous, we can use [&] -- we won't leave block until everything is done
	auto callback = [&](Conversation &c)
	{
		FINALIZE{ isDone.setn(true); };
		//logDebug("Project create call has been finished!");
		if(c.ontopicReplies.empty())
		{
			logError("Warning: conversation %s about %s has been finished before exptected message %s has been received!"
						, c.id, c.starter.topic, topicAnswer);
		}
		else if(c.ontopicReplies.size() > 1)
		{
			logError("Warning: conversation %s about %s has been finished but more than one answer received. I don't know what to do!"
						, c.id, c.starter.topic);
		}
		else
		{
			assert(c.ontopicReplies.size() == 1);
			auto firstPair = c.ontopicReplies.begin();
			auto &type = firstPair->first;
			auto &msg = firstPair->second;
			switch(type)
			{
#ifdef USES_UPDATE
			case Conversation::UPDATE:
#else
			case Conversation::STATUS:
#endif
				{
					auto answer = retrieveAnswer(msg);
					%prepare_ret%
				}
				break;
			case Conversation::EXCEPTION:
				{
					generated::proto::rpc::Exception e;
					e.ParseFromString(msg.contents);
					errorMessage = e.message();
					if(errorMessage.empty())
						errorMessage = "[No error message was provided by batch.]";
				}
				break;
			default:
				errorMessage = format_string("Conversation with correlation id=%s was finished by message of type %d. I expected something else.", c.id, type);
				break;
			}

		}
	}; //Callback end

	auto correlation = %method%_Async(%args_names_list_comma% callback);

	if(threadManager->getThreadName(boost::this_thread::get_id()) == threads::LISTENER)
	{
		while(!isDone.get())
			bh->processMessageFor(correlation);
	}
	isDone.waitWhile(false);

	if(errorMessage.size())
	{
		THROW("Request %s failed: %s", topic, errorMessage);
	}

	%return_ret%

#undef USES_%answerType%
}

CorrelationId %wrapper_name%::%method%_Async(%args_list_comma% ConversationDoneCb callback)
{
	std::string topic = "%topic%";
	std::string topicRequest = "%topic%.request";
	typedef %namespace%::%method%_Request RequestType;
	auto fillWithArgs = [&](RequestType &request)
	{
		%setters%
	};

	RequestType requestMsg;
	fillWithArgs(requestMsg);
	return sendRequest(std::move(topic), std::move(topicRequest), requestMsg, callback);
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
		{
			if(asValue)
				ret = "%2";
			else
				ret = "%1";
		}

		boost::replace_all(ret, "%1", translateBaseType(false));
		boost::replace_all(ret, "%2", translateBaseType(true));
		return ret;
	}

	std::string formatArgument() const
	{
		return translateType() + " " + arg->name();
	}
};

struct AgentWrapper
{
	std::string nameSpace;
	std::string wrapperName;

};

struct MethodWrapper
{
	std::shared_ptr<const AgentWrapper> agent;
	std::vector<const FieldDescriptor*> argsFields;
	const Descriptor *args, *result;
	const Descriptor *top;
	const EnumValueDescriptor *methodValue;

	std::string topic, name;

	std::string returnedType, returnedTypeAsync, epilogue, arguments, argumentsNames, setters;
	std::string prepareRet, returnRet, useRet, useRetAsync, defineRet;

	std::vector<int> collapsedArgs;
	std::string collapsedName; 

	MethodWrapper(std::string topic, std::string name, const FileDescriptor *file, const Descriptor *top, 
				  std::shared_ptr<const AgentWrapper> agent)
		: top(top), name(name), topic(topic), agent(agent)
	{
		args = top->FindNestedTypeByName("Request");
		result = top->FindNestedTypeByName("Status");
		if(!result) result = top->FindNestedTypeByName("Update");
		
		if(!args) return;
		for(int i = 0; i < args->field_count(); i++)
			argsFields.push_back(args->field(i));

		arguments = translateArguments();
		setters = formatSetters();
		formatEpilogue();
	}

	std::string format(const std::string &input) const
	{
		auto ret = input;
		boost::replace_all(ret, "%answerType%", boost::iequals(result->name(), "status") ? "STATUS" : "UPDATE");
		boost::replace_all(ret, "%method%", name);
		boost::replace_all(ret, "%topic%", topic);
		boost::replace_all(ret, "%namespace%", agent->nameSpace);
		boost::replace_all(ret, "%args_names_list_comma% ", argumentsNames + (arguments.size() ? ", " : ""));
		boost::replace_all(ret, "%args_list%", arguments);
		boost::replace_all(ret, "%args_list_comma%", arguments + (arguments.size() ? ", " : ""));
		boost::replace_all(ret, "%setters%", setters);
		boost::replace_all(ret, "%epilogue%", epilogue);
		boost::replace_all(ret, "%rettype%", returnedType);
		boost::replace_all(ret, "%async_rettype%", returnedTypeAsync);
		boost::replace_all(ret, "%prepare_ret%", prepareRet);
		boost::replace_all(ret, "%use_ret%", useRet);
		boost::replace_all(ret, "%async_use_ret%", useRetAsync);
		boost::replace_all(ret, "%define_ret%", defineRet);
		boost::replace_all(ret, "%return_ret%", returnRet);

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
		std::vector<std::string> names;
		for(int i = 0; i < (int)argsFields.size(); i++)
		{
			auto &arg = argsFields.at(i);
			if(arg->name() == "nodeID" && argsFields.at(i + 1)->name() == "bc")
			{
				assert(argsFields.at(i + 1)->name() == "bc");
				assert(argsFields.at(i + 2)->name() == "libraryID");
				assert(argsFields.at(i + 3)->name() == "projectID");
				assert(argsFields.at(i + 4)->name() == "_astID");
				collapsedArgs.resize(5, i);
				i += 4;
				argsTxt.push_back("const NodeId &nodeID");
				collapsedName = "nodeID";
			}
			else if(arg->name() == "bc" || arg->name() == "parentbc")
			{
				assert(argsFields.at(i + 1)->name() == "libraryID");
				assert(argsFields.at(i + 2)->name() == "projectID");

				if(arg->name() == "bc")
				{
					assert(argsFields.at(i + 3)->name() == "_astID");
					collapsedArgs.resize(4, i);
					i += 3;
					collapsedName = "defID";
				}
				else
				{
					assert(argsFields.at(i + 3)->name() == "_astID");
					collapsedArgs.resize(4, i);
					i += 3;
// 					collapsedArgs.resize(3, i);
// 					i += 2;
					collapsedName = "parent";
				}

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
				ArgWrapper aw{arg};
				collapsedName = aw.arg->name();
				argsTxt.push_back(aw.formatArgument());
			}
			names.push_back(collapsedName);
		}

		argumentsNames = boost::join(names, ", ");
		return boost::join(argsTxt, ", ");
	}

	std::string formatSetters() const
	{
		std::string ret;
		for(int i = 0; i < (int)argsFields.size(); i++)
		{
			auto &arg = argsFields.at(i);
			if(collapsedArgs.size() && collapsedArgs.front() == i)
			{
				for(int j = 0; j < (int)collapsedArgs.size(); j++)
				{
					auto &argInner = argsFields.at(i+j);

					static const std::vector<std::string> names = { "nodeID", "defID", "libID", "projID", "defID" };
					int index = 5-collapsedArgs.size()+j;
					auto derefedArg = collapsedName + "." + names[index];
					if(index == 1)
					{
						auto argLowerName = argInner->lowercase_name();
						assert(argLowerName == "bc" || argLowerName == "parentbc");
						ret += "request.mutable_" + argLowerName + "()->CopyFrom(crumbify(" + collapsedName + "));\n";
					}
					else
						ret += "request.set_" + argInner->lowercase_name() + "(" + derefedArg + ");\n";
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
									auto added = request.add_%2();
									added->MergeFrom(%1[i]);
								})";
					}
					else
					{
						entry =
							R"(	for(size_t i = 0; i < %1.size(); i++)
								{
									assert(request.%2_size() == i);
									request.add_%2(%1.at(i));
								})";
					}
				}
				else if(arg->is_optional())
					entry = "if(%1)\n{\nrequest.set_%2(*%1);\n}";
				else if(arg->type() == FieldDescriptor::TYPE_MESSAGE)
					entry = "request.mutable_%2()->CopyFrom(%1);";
				else
					entry = "request.set_%2(%1);";

				boost::replace_all(entry, "%1", arg->name());
				boost::replace_all(entry, "%2", arg->lowercase_name());
				ret += entry + "\n";
			}
		}
		return ret;
	}

	void formatEpilogue()
	{
		std::string resultPack = agent->nameSpace + "::" + name + "_" + result->name();
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
			
			if(field->is_repeated())
			{
				//prepareRet += returnedType + " ret;\n";
				prepareRet += "for(int i = 0; i < answer->"+field->lowercase_name()+"_size(); i++)\n{\n";
				prepareRet += "ret.push_back(answer->"+field->lowercase_name()+"(i));\n}\n";
			}
			else if(field->is_optional())
			{
				//prepareRet += returnedType + " ret;\n";
				prepareRet += "if(answer->has_" + field->lowercase_name() + "()) \n{\n";
				prepareRet += "ret = answer->" + field->lowercase_name() + "();\n}\n";
			}
			else if(field->type() == FieldDescriptor::TYPE_MESSAGE)
			{
				prepareRet += "auto retHlp = answer->release_" + field->lowercase_name() + "();\n";
				prepareRet += "ret = " + returnedType + "(retHlp);\n";
			}
			else
			{
				prepareRet += "ret = answer->" + field->lowercase_name() + "();\n";
			}
			returnRet = "return ret;";
			useRet = "ret";
		}
		else
		{
			returnedType = agent->nameSpace + "::" + name + "_" + result->name();
 			returnedType = "std::unique_ptr<" + returnedType + ">";
// 			prepareRet = returnedType + " retHlp;\n";
			prepareRet += "ret = std::move(answer);";
			returnRet = "return ret;";
			useRet = "ret";
		}

		if(returnedTypeAsync.empty())
			returnedTypeAsync = returnedType;
		if(useRetAsync.empty())
			useRetAsync = useRet;

		epilogue = prepareRet + "\n" + returnRet;

		if(returnedType != "void")
			defineRet = "typedef " + returnedType + " ReturnType;\nReturnType ret;";
		else
			defineRet = "";
	}
};

//methods covnerting between expr/pat/type -> cls 
std::string extToClsCovnersions()
{
	std::string ret;

	std::vector<std::string> toHandle = { "Expr", "Pat", "Type" };
	auto d = generated::proto::projectManager::Project::descriptor();
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

					std::cout << "\t\t" << enumVal->name() << std::endl;
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

void prepareMethodWrappersHelper(bool finalLeaves, std::vector<MethodWrapper> &methods,
								 const google::protobuf::Descriptor *descriptor)
{
	auto fileDescriptor = descriptor->file();
	auto agent = make_shared<AgentWrapper>();
	agent->wrapperName = fileDescriptor->package().substr(fileDescriptor->package().find_last_of(".") + 1);
	agent->nameSpace = fileDescriptor->package();
	boost::replace_all(agent->nameSpace, ".", "::");

	std::function<void(std::string, std::string, const google::protobuf::Descriptor *)> addMethodsRecursively =
		[&](std::string topicSoFar, std::string nameSoFar, const google::protobuf::Descriptor *d)
	{
		if(topicSoFar.size())
		{
			topicSoFar += "." + boost::to_lower_copy(d->name());
			nameSoFar += "_" + d->name();
		}
		else
		{
			topicSoFar = boost::to_lower_copy(d->name());
			nameSoFar = d->name();
		}

		if(finalLeaves)
		{
			if(d->name() == "Request" || d->name() == "Status" || d->name() == "Update")
				methods.emplace_back(topicSoFar, nameSoFar, fileDescriptor, d, agent);
		}
		else
		{
			if(d->FindNestedTypeByName("Request"))
				methods.emplace_back(topicSoFar, nameSoFar, fileDescriptor, d, agent);
		}
		for(int i = 0; i < d->nested_type_count(); i++)
		{
			auto nested = d->nested_type(i);
			addMethodsRecursively(topicSoFar, nameSoFar, nested);
		}
	};

	addMethodsRecursively("", "", descriptor);
}

std::vector<MethodWrapper> prepareMethodWrappers(bool finalLeaves = false)
{
	std::vector<MethodWrapper> methods;
	prepareMethodWrappersHelper(finalLeaves, methods, generated::proto::projectManager::Project::descriptor());
	prepareMethodWrappersHelper(finalLeaves, methods, generated::proto::parser::Parse::descriptor());
	prepareMethodWrappersHelper(finalLeaves, methods, generated::proto::fileManager::FileSystem::descriptor());
	prepareMethodWrappersHelper(finalLeaves, methods, generated::proto::pluginManager::Plugin::descriptor());
	return methods;
}

void generate(path outputFile)
{
	std::string methodImpls;
	std::string methodDecls;
	
	auto methods = prepareMethodWrappers();

// 	auto methodsDescriptor = Request::Method_descriptor();
	for(auto &method : methods)
	{
		std::cout << "Handling method " << method.name << std::endl;
		methodImpls += method.formatImpl();
		methodDecls += method.formatDecl() + "\n";
	}

	auto formatFile = [&](const std::string &input) -> std::string
	{
		auto ret = input;
		boost::replace_all(ret, "%method_decls%", methodDecls);
		boost::replace_all(ret, "%method_impls%", methodImpls);
		boost::replace_all(ret, "%wrapper_name%", outputFile.string().substr(outputFile.string().find_last_of('/') + 1));
		boost::replace_all(ret, "%ext_to_enum%", extToClsCovnersions());
		return ret;
	};

	auto formatAndWrite = [&](const path &outfile, const std::string &preformattedFile)
	{
		std::cout << "Formatting " << outfile << std::endl;
		auto formattedText = formatFile(preformattedFile);
		std::cout << "Formatting output " << outfile << std::endl;
		boost::filesystem::ofstream out(outfile);
		assert(out);
		formatOutput(out, formattedText);
	};

	formatAndWrite(outputFile.string() + ".cpp", sourceFile);
	formatAndWrite(outputFile.string() + ".h", headerFile);
}

void generateDeserializers()
{
	std::cout << "Generating deserializers..." << std::endl;
	static const std::string header = R"(
#pragma once 

struct PackageDeserializer
{
	static std::unique_ptr<google::protobuf::Message> deserialize(const BusMessage &message);
};
)";
	std::string body = R"(

#include "../bus/BusLibrary.h"
#include "Deserializer.h"
#include <google/protobuf/message.h>


typedef std::unique_ptr<google::protobuf::Message> MessagePtr;

std::unique_ptr<google::protobuf::Message> PackageDeserializer::deserialize(const BusMessage &message)
{
	static const std::map<std::string, std::function<MessagePtr(crstring)>> deserializers = 
	{
%deserializers%
	};

	auto deserializerItr = deserializers.find(message.topic);
	if(deserializerItr != deserializers.end())
		return deserializerItr->second(message.contents);

	if(boost::algorithm::ends_with(message.topic, ".error"))
	{
		auto ret = make_unique<generated::proto::rpc::Exception>();
		ret->set_message(message.contents);
//TODO //FIXME Why move is needed?
		return std::move(ret);
	}

	if(/*boost::ends_with(message.topic, "request") || */boost::starts_with(message.topic, "builder.")  ||  boost::starts_with(message.topic, "test."))
		// No action needed for our packs
		return nullptr;

	THROW("Error: I don't know how to deserialize message with topic %s.", message.topic);
}

)";
	std::string deserialize = R"(
		{
			"%topic%", 
			[](crstring contents)
			{
				auto ret = make_unique<%namespace%::%method%>();
				ret->ParseFromString(contents);
				return ret;
			}
		},
)";
	
	std::string entries;

	auto methods = prepareMethodWrappers(true);
	for (auto &method : methods)
	{
		std::string hlp = deserialize;
		boost::replace_all(hlp, "%namespace%", method.agent->nameSpace);
		boost::replace_all(hlp, "%topic%", method.topic);
		boost::replace_all(hlp, "%method%", method.name);
		entries += hlp;
	}

	std::string output = body;
	boost::replace_all(output, "%deserializers%", entries);

	boost::filesystem::ofstream outcpp(outputDirectory / "Deserializer.cpp");
	boost::filesystem::ofstream outh(outputDirectory / "Deserializer.h");
	formatOutput(outh, header);
	formatOutput(outcpp, output);
}

void generateDispatcher()
{
	std::cout << "Generating dispatcher..." << std::endl;
	std::string source = R"(
#include "MessageDispatcher.h"
#include "../bus/BusLibrary.h"


void MessageDispatcher::dispatch(const BusMessage &message, IDispatchee &dispatchee)
{
	static const std::map<std::string, std::function<void(const BusMessage &, IDispatchee &dispatchee)>> deserializers =
	{
		%elements%
	};

	auto deserializerItr = deserializers.find(message.topic);
	if(deserializerItr != deserializers.end())
	{
		deserializerItr->second(message, dispatchee);
		return;
	}


	if(!boost::ends_with(message.topic, ".request"))
		logWarning("Not dispatching message %s.", message.topic);;
}

void IBusMessagesReceiver::handle(const BusMessage &message)
{
	MessageDispatcher::dispatch(message, *this);
}

)";

	std::string header = R"(
#pragma once

#include "../bus/BusListener.h"

struct BusMessage;

%dispatchee%

struct MessageDispatcher
{
	static void dispatch(const BusMessage &message, IDispatchee &dispatchee);
};

struct IBusMessagesReceiver : IBusListener, IDispatchee
{
	virtual void handle(const BusMessage &message);
};

)";
	std::string entry = R"(
			{ 
				"%topic%",
				[](const BusMessage &message, IDispatchee &dispatchee)
				{
					typedef %namespace%::%method% MyMsg;
					dispatchee.on_%method%(message, std::dynamic_pointer_cast<const MyMsg>(message.msg));
				}
			},
)";

	std::string entries;
	std::string dispatchee = "class IDispatchee \n{\n public:\n";
	for(auto &method : prepareMethodWrappers(true))
{	
		if(!boost::ends_with(method.topic, "update")  &&  !boost::ends_with(method.topic, "status"))
			continue;

		std::string hlp = entry;
		boost::replace_all(hlp, "%namespace%", method.agent->nameSpace);
		boost::replace_all(hlp, "%topic%", method.topic);
		boost::replace_all(hlp, "%method%", method.name);
		entries += hlp;
		dispatchee += "virtual void on_"+method.name+"(const BusMessage &message, const std::shared_ptr<const " + method.agent->nameSpace + "::"+method.name+"> &ptr) {};\n";
	}
	dispatchee += "}; \n";

	boost::replace_all(header, "%dispatchee%", dispatchee);
	boost::replace_all(source, "%elements%", entries);

	boost::filesystem::ofstream outcpp(outputDirectory / "MessageDispatcher.cpp");
	boost::filesystem::ofstream outh(outputDirectory /"MessageDispatcher.h");
	formatOutput(outh, header);
	formatOutput(outcpp, source);
	//topic project.library.ast.properties.set.update
	//method Project_Library_AST_Properties_Set_Update

}

int main()
{
//	extToClsCovnersions();
	generate(outputDirectory / "ProjectManager");
	generateDeserializers();
	generateDispatcher();
	return EXIT_SUCCESS;
}