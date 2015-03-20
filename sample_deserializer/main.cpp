#include "main.h"

#include <fstream>

int main()
{
	std::ifstream input{ "test.bin", std::ios::binary };
	auto blah = readPrimitive<std::int64_t>(input);

	if(!input)
		return 1;

	auto moje = Expr::deserializeFrom(input);
	return 0;
}

std::shared_ptr<Expr> Expr::deserializeFrom(Input &input)
{
	auto constructorIndex = readInt8(input);
	switch(constructorIndex)
	{
	case 0: return ConstructorSelector<Expr, 0>::ConstructorType::deserializeFrom(input);
	case 1: return ConstructorSelector<Expr, 1>::ConstructorType::deserializeFrom(input);
	case 10: return ConstructorSelector<Expr, 10>::ConstructorType::deserializeFrom(input);
	default: return nullptr;
	}
}

std::shared_ptr<Expr_NOP> Expr_NOP::deserializeFrom(Input &input)
{
	auto ret = std::make_shared<Expr_NOP>();
	ret->_id = readInt64(input);
	return ret;
}

std::shared_ptr<Expr_Accessor> Expr_Accessor::deserializeFrom(Input &input)
{
	auto ret = std::make_shared<Expr_Accessor>();
	ret->_id = readInt64(input);
// 	Accessor::deserializeFrom(ret->_acc);
// 	Expr::deserializeFrom(ret->_dst);
	return ret;
}

std::shared_ptr<Expr_Con> Expr_Con::deserializeFrom(Input &input)
{
	auto ret = std::make_shared<Expr_Con>();
	ret->_id = readInt64(input);
	decode(ret->name, input);
	return ret;
}
