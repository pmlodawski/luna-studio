#include "helper.h"
#include "generated.h"
#include "main.h"

#include <fstream>

using Expr = Expr_Expr;
using Expr_Con = Expr_Expr_Con;
using Accessor_ConAccessor = Expr_Accessor_ConAccessor;

std::shared_ptr<Expr> parseFile(const std::string &fname)
{
	std::ifstream in{fname, std::ios::binary };
	if(!in) 
		throw std::runtime_error("Failed to open the file " + fname);

	try
	{
		in.exceptions(std::ios::badbit | std::ios::failbit | std::ios::eofbit);
		return Expr::deserializeFrom(std::ifstream{fname, std::ios::binary });
	}
	catch(std::exception &e)
	{
		throw std::runtime_error("Failed when reading file " + fname + " : " + e.what());
	}
}

int main()
{
	try
	{
	std::ifstream input{ "test.bin", std::ios::binary };
	//std::int64_t blah = readPrimitive<std::int64_t>(input);

	if(!input)
		return 1;

	auto moje = Expr::deserializeFrom(input);

	auto acc = std::make_shared<Accessor_ConAccessor>();
	acc->_accName = "bar";

	auto con = std::make_shared<Expr_Con>();
	con->_id = 502;
	con->_name = "foo";

	auto moje2 = std::make_shared<Expr_Expr_Accessor>();
	moje2->_id = 503;
	moje2->_dst = con;
	moje2->_acc = acc;

	auto testimp = Expr::deserializeFrom(std::ifstream{ "testimp.bin", std::ios::binary });
	auto argexp = parseFile("testargexp.bin");

	{
		std::ofstream output{ "testout0.bin", std::ios::binary };
		//serialize(blah, output);
		moje->serialize(output);
	}

	{
		std::ofstream output{ "testout.bin", std::ios::binary };
		//serialize(blah, output);
		moje2->serialize(output);
	}
	}
	catch(std::exception &e)
	{
		std::cout << "Exception encountered: " << e.what() << std::endl;
	}
	return 0;
}

// std::shared_ptr<Expr> Expr::deserializeFrom(Input &input)
// {
// 	auto constructorIndex = readInt8(input);
// 	switch(constructorIndex)
// 	{
// 	case 0: return ConstructorSelector<Expr, 0>::ConstructorType::deserializeFrom(input);
// 	case 1: return ConstructorSelector<Expr, 1>::ConstructorType::deserializeFrom(input);
// 	case 10: return ConstructorSelector<Expr, 10>::ConstructorType::deserializeFrom(input);
// 	default: return nullptr;
// 	}
// }
// 
// std::shared_ptr<Expr_NOP> Expr_NOP::deserializeFrom(Input &input)
// {
// 	auto ret = std::make_shared<Expr_NOP>();
// 	ret->_id = readInt64(input);
// 	return ret;
// }
// 
// std::shared_ptr<Expr_Accessor> Expr_Accessor::deserializeFrom(Input &input)
// {
// 	auto ret = std::make_shared<Expr_Accessor>();
// 	ret->_id = readInt64(input);
// // 	Accessor::deserializeFrom(ret->_acc);
// // 	Expr::deserializeFrom(ret->_dst);
// 	return ret;
// }
// 
// std::shared_ptr<Expr_Con> Expr_Con::deserializeFrom(Input &input)
// {
// 	auto ret = std::make_shared<Expr_Con>();
// 	ret->_id = readInt64(input);
// 	deserialize(ret->name, input);
// 	return ret;
// }
