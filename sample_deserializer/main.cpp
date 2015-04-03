#include "helper.h"
#include "generated.h"
#include "main.h"

#include <fstream>

using Expr = Generator_Expr_Expr;
using Expr_Con = Generator_Expr_Expr_Con;
using Accessor_ConAccessor = Generator_Expr_Accessor_ConAccessor;

template<typename T = Expr>
std::shared_ptr<T> parseFile(const std::string &fname)
{
	std::ifstream in{fname, std::ios::binary };
	if(!in) 
		throw std::runtime_error("Failed to open the file " + fname);

	try
	{
		in.exceptions(std::ios::badbit | std::ios::failbit | std::ios::eofbit);
		return T::deserializeFrom(std::ifstream{fname, std::ios::binary });
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

	auto moje2 = std::make_shared<Generator_Expr_Expr_Accessor>();
	moje2->_id = 503;
	moje2->_dst = con;
	moje2->_acc = acc;

	std::int64_t mantissa = 8285044862877696;
	int exponent = -26;
// 	mantissa = std::frexp(123456789, &exponent);
// 
// 	auto tttt=  std::ldexp(8285044862877696,-26);

	auto testimp = Expr::deserializeFrom(std::ifstream{ "testimp.bin", std::ios::binary });
	auto argexp = parseFile("testargexp.bin");
	auto argexp2 = parseFile<Generator_Expr_Name>("testname.bin");
	auto testlit = parseFile<Expr>("testlit.bin");
	if(auto a = std::dynamic_pointer_cast<Generator_Expr_Name_NameA>(argexp2))
	{
		auto bbbb = swap_endian(a->field_2);

	}


	{
		std::ofstream output{ "testlit0.bin", std::ios::binary };
		testlit->serialize(output);
	}
	{
		std::ofstream output{ "testimp0.bin", std::ios::binary };
		testimp->serialize(output);
	}

	{
		std::ofstream output{ "testout0.bin", std::ios::binary };
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
