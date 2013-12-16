#include <fstream>

#include "generated/server-api.pb.h"
#include <boost/algorithm/string.hpp>

#ifdef _WIN32
#pragma comment(lib, "libprotobuf.lib")
#endif


using namespace generated::proto::batch;
using namespace google::protobuf;

const std::string primaryWrapper = R"(class %1
{
public:

generated::proto::batch::Response callAndTranslateException(const Request& request)
{
	return ::callAndTranslateException(socket, request);
}

tcp::socket &socket;
%1(tcp::socket &socket) : socket(socket) {}
%2
};
)";

const std::string methodWrapper = R"(
%6 %1_%2(%3)
{
	try
	{
		generated::proto::batch::%1_%2_Args *args = new generated::proto::batch::%1_%2_Args();
	%4

		Request request;
		request.set_method(Request_Method_%1_%2);
		request.SetAllocatedExtension(generated::proto::batch::%1_%2_Args::req, args);

		generated::proto::batch::Response response = callAndTranslateException(request);
		assert(response.type() == Response_Type_Result); //exception would be translated to exception

	%5
	}
	catch(std::exception &e)
	{
		std::string msg = std::string("Call to batch method %1::%2 triggered an exception: ") + e.what();
		throw std::runtime_error(msg);
	}
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
				ret = "%1 *";
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

	std::string returnedType, epilogue;


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

		formatEpilogue();
	}

	std::string format() const 
	{
		auto ret = methodWrapper;
		boost::replace_all(ret, "%1", space);
		boost::replace_all(ret, "%2", name);
		boost::replace_all(ret, "%3", translateArguments());
		boost::replace_all(ret, "%4", formatSetters());
		boost::replace_all(ret, "%5", epilogue);
		boost::replace_all(ret, "%6", returnedType);
		return ret;
	}

	std::string translateArguments() const 
	{
		std::vector<std::string> argsTxt;
		for(auto arg : argsFields)
			argsTxt.push_back(ArgWrapper(arg).formatArgument());

		return boost::join(argsTxt, ", ");
	}

	std::string formatSetters() const
	{
		std::string ret;
		for(auto arg : argsFields)
		{
			std::string entry;
			if(arg->is_repeated())
			{
				if(arg->type() == FieldDescriptor::TYPE_MESSAGE)
				{
					entry = "\tfor(size_t i = 0; i < %1.size(); i++)\n"
					"\t{\n"
						"\t\tauto added = args->add_%2();\n"
						"\t\tadded->MergeFrom(*args);\n"
						"\t\tdelete args;\n"
					"\t}\n";
				}
				else
				{
					entry = "\tfor(size_t i = 0; i < %1.size(); i++)\n"
						"\t\targs->set_%2(i, %1.at(i));";
				}
			}
			else if(arg->is_optional())
				entry = "\tif(%1)\n"
				"\t\targs->set_%2(*%1);";
			else if(arg->type() == FieldDescriptor::TYPE_MESSAGE)
				entry = "\targs->set_allocated_%2(%1);";
			else
				entry = "\targs->set_%2(%1);";

			boost::replace_all(entry, "%1", arg->name());
			boost::replace_all(entry, "%2", arg->lowercase_name());
			ret += entry + "\n";
		}
		return ret;
	}

	void formatEpilogue()
	{
		auto resultPack = "generated::proto::batch::" + space + "_" + name + "_Result";
		if(result->field_count() == 0)
		{
			returnedType = "void";
			epilogue = "\treturn;";
		}
		else if(result->field_count() == 1)
		{
			auto field = result->field(0);
			returnedType = ArgWrapper(result->field(0)).translateType(true);
			epilogue = "\t" + resultPack + " result = response.GetExtension(" + resultPack + "::rsp);\n";
			
			if(field->is_repeated())
			{
				epilogue += "\t" + returnedType + " ret;\n";
				epilogue += "\tfor(int i = 0; i < result."+field->lowercase_name()+"_size(); i++)\n";
				epilogue += "\t\tret.push_back(result."+field->lowercase_name()+"(i));\n";
				epilogue += "\treturn ret;";
			}
			else
			{
				epilogue += "\treturn result." + field->lowercase_name() + "();";
			}
		}
		else
		{
			returnedType = "generated::proto::batch::" + space + "_" + name + "_Result";
			epilogue = "\treturn response.GetExtension(" + returnedType + "::rsp);";
		}
	}
};


void generate(const std::string &outputFile)
{
	std::string methodsList;

	auto fileDescriptor = AST::descriptor()->file();
	auto methodsDescriptor = Request::Method_descriptor();
	for(int i = 0; i < methodsDescriptor->value_count(); i++)
	{
		methodsList += MethodWrapper(fileDescriptor, methodsDescriptor->value(i)).format();
	}

	std::string output = primaryWrapper;
	boost::replace_all(output, "%1", "Wrapper");
	boost::replace_all(output, "%2", methodsList);

	std::ofstream out(outputFile);
	out << output << std::flush;

	for(int i = 0; i < fileDescriptor->message_type_count(); i++)
	{
		auto messageDescriptor = fileDescriptor->message_type(i);
		int g = 4;
	}
}

int main()
{
	generate("out.cpp");
	return EXIT_SUCCESS;
}