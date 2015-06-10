#include <vector>
#include <memory>
#include <cstdint>
#include <iostream>

// 
// 
// class Accessor;
// 
// template  <typename Type, int index>
// struct ConstructorSelector
// {};
// 
// 
// class Expr
// {
// public:
// 	Expr() {}
// 	virtual ~Expr() {}
// 
// 	static const int ConstructorCount = 2;
// 
// 	static std::shared_ptr<Expr> deserializeFrom(Input &input);
// 
// };
// 
// class Expr_NOP : public Expr
// {
// public:
// 	ID _id;
// 
// 
// 	static std::shared_ptr<Expr_NOP> deserializeFrom(Input &input);
// };
// 
// template<>
// struct ConstructorSelector < Expr, 0 >
// {
// 	typedef Expr_NOP ConstructorType;
// };
// 
// class Expr_Accessor : public Expr
// {
// public:
// 	ID _id;
// 	std::shared_ptr<Accessor> _acc;
// 	std::shared_ptr<Expr> _dst;
// 
// 	static std::shared_ptr<Expr_Accessor> deserializeFrom(Input &input);
// };
// 
// template<>
// struct ConstructorSelector < Expr, 1 >
// {
// 	typedef Expr_Accessor ConstructorType;
// };
// 
// 
// class Expr_Con : public Expr
// {
// public:
// 	ID _id;
// 	std::string name;
// 
// 	static std::shared_ptr<Expr_Con> deserializeFrom(Input &input);
// };
// 
// template<>
// struct ConstructorSelector <Expr, 10>
// {
// 	typedef Expr_Con ConstructorType;
// };
// 
// 
// 
// class Accessor
// {
// public:
// 	static std::shared_ptr<Accessor> deserializeFrom(Input &input);
// };
// 
// class Accessor_VarAccessor : public Accessor
// {
// public:
// 	std::string _accName;
// 
// 	static std::shared_ptr<Accessor_VarAccessor> deserializeFrom(Input &input)
// 	{
// 		auto ret = std::make_shared<Accessor_VarAccessor>();
// 		deserialize(ret->_accName, input);
// 		return ret;
// 	}
// };
// 
// class Accessor_ConAccessor : public Accessor
// {
// public:
// 	std::string _accName;
// 
// 	static std::shared_ptr<Accessor_ConAccessor> deserializeFrom(Input &input)
// 	{
// 		auto ret = std::make_shared<Accessor_ConAccessor>();
// 		deserialize(ret->_accName, input);
// 		return ret;
// 	}
// };
