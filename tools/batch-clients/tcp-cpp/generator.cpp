#include <fstream>

#include "generated/server-api.pb.h"
#include "generated/project-manager.pb.h"
#include <boost/algorithm/string.hpp>
#include <boost/filesystem.hpp>

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
	for(int i = 0; i < (int)hlp.size(); i++)
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


#include "../BatchIdWrappers.h"

class BusHandler;

class %wrapper_name%
{
public:
	BusHandler *bh;

	void sendRequest(std::string baseTopic, std::string requestTopic, const google::protobuf::Message &msg, ConversationDoneCb callback);

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

	%wrapper_name%(BusHandler *bh) : bh(bh) {}
	%method_decls%
};

%ext_to_enum%

)";

const std::string sourceFile = R"(
#include "stdafx.h"
#include "BusLibrary.h"
#include "BatchClient.h"

#include <google/protobuf/io/zero_copy_stream_impl.h>
#include <google/protobuf/io/coded_stream.h>


void %wrapper_name%::sendRequest(std::string baseTopic, std::string requestTopic, const google::protobuf::Message &msg, ConversationDoneCb callback)
{
	bh->request(std::move(baseTopic), std::move(requestTopic), msg.SerializeAsString(), callback);
}

%method_impls%

)";

const std::string methodDeclaration = "%rettype% %method%(%args_list%);";
const std::string methodDeclarationAsync = "void %method%_Async(%args_list_comma% ConversationDoneCb callback);";

const std::string methodDefinition = R"(
%rettype% %wrapper_name%::%method%(%args_list%)
{
	LOG_TIME("%topic% -- request in total");
	std::string topic = "%topic%";

#define USES_%answerType%

#ifdef USES_UPDATE
	std::string topicAnswer = "%topic%.update";
	typedef generated::proto::projectManager::%method%_Update AnswerType;
#else
	std::string topicAnswer = "%topic%.status";
	typedef generated::proto::projectManager::%method%_Status AnswerType;
#endif
	typedef %rettype% ReturnType;

	auto retrieveAnswer = [](const BusMessage &bm)
	{
		auto ret = make_unique<AnswerType>();
		ret->ParseFromString(bm.contents);
		return ret;
	};

	boost::barrier b{2};
	ReturnType ret;
	std::string errorMessage;

	// Because we are synchronous, we can use [&] -- we won't leave block until everything is done
	auto callback = [&](Conversation &c)
	{
		FINALIZE{ b.wait(); };
		logDebug("Project create call has been finished!");
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
				logError("Conversation with correlation id=%s was finished by message of type %d. I expected something else.", c.id, type);
			}

		}
	}; //Callback end

	%method%_Async(%args_names_list_comma% callback);
	b.wait();

	if(errorMessage.size())
		THROW("Request %s failed: %s", "project.create", topic, errorMessage);

	return ret;

#undef USES_%answerType%
}


void %wrapper_name%::%method%_Async(%args_list_comma% ConversationDoneCb callback)
{
	std::string topic = "%topic%";
	std::string topicRequest = "%topic%.request";
	typedef generated::proto::projectManager::%method%_Request RequestType;
	auto fillWithArgs = [&](RequestType &request)
	{
		%setters%
	};

	RequestType requestMsg;
	fillWithArgs(requestMsg);
	sendRequest(std::move(topic), std::move(topicRequest), requestMsg, callback);
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
	const Descriptor *top;
	const EnumValueDescriptor *methodValue;

	std::string topic, name;

	std::string returnedType, returnedTypeAsync, epilogue, arguments, argumentsNames, setters;
	std::string prepareRet, returnRet, useRet, useRetAsync;

	std::vector<int> collapsedArgs;
	std::string collapsedName; 

	MethodWrapper(std::string topic, std::string name, const FileDescriptor *file, const Descriptor *top) : top(top)
		, name(name), topic(topic)
	{
		args = top->FindNestedTypeByName("Request");
		result = top->FindNestedTypeByName("Status");
		if(!result) result = top->FindNestedTypeByName("Update");
		
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

					static const std::string names[] = { "nodeID", "defID", "libID", "projID" };
					int index = 4-collapsedArgs.size()+j;
					auto derefedArg = collapsedName + "." + names[index];
					if(index == 1)
					{
						ret += "request.mutable_" + argInner->lowercase_name() + "()->CopyFrom(crumbify(" + collapsedName + "));\n";
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
		std::string resultPack = "generated::proto::projectManager::"+name+"_" + result->name();
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
			returnedType = "generated::proto::projectManager::" + name + "_" + result->name();
			returnedType = "std::unique_ptr<" + returnedType + ">";
			prepareRet = returnedType + " ret;\n";
			prepareRet += "ret = std::move(answer);";
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

	std::vector<MethodWrapper> methods;
	auto fileDescriptor = generated::proto::projectManager::Project::descriptor()->file();
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

		if(d->FindNestedTypeByName("Request"))
			methods.emplace_back(topicSoFar, nameSoFar, fileDescriptor, d);

		for(int i = 0; i < d->nested_type_count(); i++)
		{
			auto nested = d->nested_type(i);
			addMethodsRecursively(topicSoFar, nameSoFar, nested);
		}
	};

	addMethodsRecursively("", "", generated::proto::projectManager::Project::descriptor());



// 	auto methodsDescriptor = Request::Method_descriptor();
	for(auto &method : methods)
	{
		methodImpls += method.formatImpl();
		methodDecls += method.formatDecl() + "\n";
	}

	auto formatFile = [&](const std::string &input) -> std::string
	{
		auto ret = input;
		boost::replace_all(ret, "%method_decls%", methodDecls);
		boost::replace_all(ret, "%method_impls%", methodImpls);
		boost::replace_all(ret, "%wrapper_name%", outputFile.substr(outputFile.find_last_of('/') + 1));
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
//	extToClsCovnersions();
	generate("generated/ProjectManager");
	return EXIT_SUCCESS;
}