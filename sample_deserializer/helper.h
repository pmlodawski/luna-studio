#include <vector>
#include <memory>
#include <cstdint>
#include <iostream>
#include <boost/optional.hpp>

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

inline void deserialize(std::int8_t &val, Input &input)
{
	val = readInt8(input);
}

inline void deserialize(std::int16_t &val, Input &input)
{
	val = readInt16(input);
}

inline void deserialize(std::int32_t &val, Input &input)
{
	val = readInt32(input);
}

inline void deserialize(std::int64_t &val, Input &input)
{
	val = readInt64(input);
}

template<typename T>
inline void deserialize(std::shared_ptr<T> &val, Input &input)
{
	val = typename T::deserializeFrom(input);
}

template<typename T>
inline void deserialize(std::vector<T> &out, std::istream &input)
{
	auto size = readInt64(input);
	out.resize(size);
	for(int i = 0; i < size; i++)
	{
		deserialize(out[i], input);
	}
}

inline void deserialize(std::string &out, std::istream &input)
{
	auto size = readInt64(input);
	out.resize(size);
	for(int i = 0; i < size; i++)
	{
		out[i] = readInt8(input);
	}
}

template<typename T>
inline void deserialize(boost::optional<T> &out, std::istream &input)
{
	int nonempty = readInt8(input);
	if(nonempty)
	{
		out = T();
		deserialize(*out, input);
	}
	else
		out = boost::none;
}