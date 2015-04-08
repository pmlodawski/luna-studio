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
		auto ret = T::deserializeFrom(std::ifstream{ fname, std::ios::binary });
		{
			// Write the file back so the contents can be compared
			std::ofstream output{ fname + "2", std::ios::binary };
			ret->serialize(output);
		}
		return ret;
	}
	catch(std::exception &e)
	{
		throw std::runtime_error("Failed when reading file " + fname + " : " + e.what());
	}
}
 
//template<std::size_t> struct int_ {};

template <class Tuple, size_t Pos>
std::ostream& print_tuple(std::ostream& out, const Tuple& t, int_<Pos>)
{
	out << std::get< std::tuple_size<Tuple>::value - Pos >(t) << ',';
	return print_tuple(out, t, int_<Pos - 1>());
}

template <class Tuple>
std::ostream& print_tuple(std::ostream& out, const Tuple& t, int_<1>)
{
	return out << std::get<std::tuple_size<Tuple>::value - 1>(t);
}

template <class... Args>
std::ostream& operator<<(std::ostream& out, const std::tuple<Args...>& t)
{
	out << '(';
	print_tuple(out, t, int_<sizeof...(Args)>());
	return out << ')';
}


int main()
{
	auto t1 = std::make_tuple("Foo", 1, 3.5);
	std::cout << t1 << std::endl; // => (2,3.4f,"awesomeness")
	try
	{
	std::ifstream input{ "test.bin", std::ios::binary };
	//std::int64_t blah = readPrimitive<std::int64_t>(input);

	if(!input)
		return 1;

	auto moje = Expr::deserializeFrom(input);


	auto testimp = Expr::deserializeFrom(std::ifstream{ "testimp.bin", std::ios::binary });
	auto argexp = parseFile("testargexp.bin");
	auto argexp2 = parseFile<Generator_Expr_Name>("testname.bin");
	auto testlit = parseFile<Expr>("testlit.bin");
	auto testtuple = parseFile<Expr>("testref.bin");
	if(auto a = std::dynamic_pointer_cast<Generator_Expr_Name_NameA>(argexp2))
	{
		auto bbbb = swap_endian(a->field_2);

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
