#include <vector>
#include <memory>
#include <cstdint>
#include <iostream>
typedef std::int32_t ID;
typedef std::istream Input;

template <typename T>
inline T swap_endian(T u)
{
	static_assert (CHAR_BIT == 8, "CHAR_BIT != 8");

	union
	{
		T u;
		unsigned char u8[sizeof(T)];
	} source, dest;

	source.u = u;

	for(size_t k = 0; k < sizeof(T); k++)
		dest.u8[k] = source.u8[sizeof(T) - k - 1];

	return dest.u;
}

template<typename T>
inline T readPrimitive(std::istream &input)
{
	T ret;
	input.read((char*)&ret, sizeof(ret));
	ret = swap_endian(ret);
	return ret;
}

inline std::int64_t readInt64(std::istream &input)
{
	return readPrimitive<std::int64_t>(input);
}

inline std::int32_t readInt32(std::istream &input)
{
	return readPrimitive<std::int32_t>(input);
}

inline std::int16_t readInt16(std::istream &input)
{
	return readPrimitive<std::int16_t>(input);
}

inline std::int8_t readInt8(std::istream &input)
{
	return readPrimitive<char>(input);
}

template<typename T>
inline void decode(std::shared_ptr<T> &out, std::istream &input)
{
	auto ret = std::make_shared<T>();
	decode(*ret, input);
	out = ret;
}

template<typename T>
inline void decode(std::vector<T> &out, std::istream &input)
{
	auto size = readInt64(input);
	out.resize(size);
	for(int i = 0; i < size; i++)
	{
		decode(out[i], input);
	}
}

inline void decode(std::string &out, std::istream &input)
{
	auto size = readInt64(input);
	out.resize(size);
	for(int i = 0; i < size; i++)
	{
		out[i] = readInt8(input);
	}
}

class Accessor;

template  <typename Type, int index>
struct ConstructorSelector
{};


class Expr
{
public:
	Expr() {}
	virtual ~Expr() {}

	static const int ConstructorCount = 2;

	static std::shared_ptr<Expr> deserializeFrom(Input &input);

};

class Expr_NOP : public Expr
{
public:
	ID _id;


	static std::shared_ptr<Expr_NOP> deserializeFrom(Input &input);
};

template<>
struct ConstructorSelector < Expr, 0 >
{
	typedef Expr_NOP ConstructorType;
};

class Expr_Accessor : public Expr
{
public:
	ID _id;
	std::shared_ptr<Accessor> _acc;
	std::shared_ptr<Expr> _dst;

	static std::shared_ptr<Expr_Accessor> deserializeFrom(Input &input);
};

template<>
struct ConstructorSelector < Expr, 1 >
{
	typedef Expr_Accessor ConstructorType;
};


class Expr_Con : public Expr
{
public:
	ID _id;
	std::string name;

	static std::shared_ptr<Expr_Con> deserializeFrom(Input &input);
};

template<>
struct ConstructorSelector <Expr, 10>
{
	typedef Expr_Con ConstructorType;
};



class Accessor
{
public:
	static std::shared_ptr<Accessor> deserializeFrom(Input &input);
};

class Accessor_VarAccessor : public Accessor
{
public:
	std::string _accName;

	static std::shared_ptr<Accessor_VarAccessor> deserializeFrom(Input &input)
	{
		auto ret = std::make_shared<Accessor_VarAccessor>();
		decode(ret->_accName, input);
		return ret;
	}
};

class Accessor_ConAccessor : public Accessor
{
public:
	std::string _accName;

	static std::shared_ptr<Accessor_ConAccessor> deserializeFrom(Input &input)
	{
		auto ret = std::make_shared<Accessor_ConAccessor>();
		decode(ret->_accName, input);
		return ret;
	}
};
