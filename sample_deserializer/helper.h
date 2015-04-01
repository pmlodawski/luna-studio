#include <vector>
#include <memory>
#include <cstdint>
#include <iostream>
#include <string>
#include <boost/optional.hpp>

typedef std::istream Input;
typedef std::ostream Output;

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

inline void deserialize(std::int8_t &val, Input &input);
template<typename T>
inline void deserialize(std::vector<T> &out, std::istream &input);

inline long long readInteger(Input &input)
{
	bool isBig = readInt8(input);
	if(!isBig)
	{
		return readInt32(input);
	}
	else
	{
		std::vector<std::int8_t> bytes;

		int signum = readInt8(input);
		deserialize(bytes, input);

		if(bytes.size() > 7)
		{
			std::runtime_error("Cannot deserialize " + std::to_string(bytes.size()) + " bytes long Integer!");
		}

		std::int64_t ret = 0;
		for(int i = 0; i < bytes.size(); i++)
		{
			ret = ret << 8;
			std::uint8_t byte = bytes[bytes.size() - 1 - i];
			ret += byte;
			//ret += (std::uint8_t) bytes[/*bytes.size() - 1 -*/ i];
		}

		if(signum < 0)
			return -ret;

		return ret;
	}
}

template<typename T>
T readFloatingPoint(Input &input)
{
	T ret = 0;
	double mantissa = readInteger(input);
	double exponent = readInt64(input);
	ret = std::ldexp(mantissa, exponent);
	return ret;
}

inline void deserialize(char &val, Input &input)
{
	val = readInt8(input);
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

inline void deserialize(float &val, Input &input)
{
	val = readFloatingPoint<float>(input);
}

inline void deserialize(double &val, Input &input)
{
	val = readFloatingPoint<double>(input);
}

template<typename T>
inline void deserialize(std::shared_ptr<T> &val, Input &input)
{
	val = typename T::deserializeFrom(input);
}

template<typename T>
inline void deserializeMaybe(std::shared_ptr<T> &val, Input &input)
{
	auto isNonEmpty = readInt8(input);
	if(isNonEmpty)
	{
		val = typename T::deserializeFrom(input);
	}
	else
	{
		val = nullptr;
	}
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

template<typename T>
inline void writePrimitive(std::ostream &output, const T &value)
{
	const auto fixedEndian = swap_endian(value);
	const auto length = sizeof(fixedEndian);
	output.write((const char *)&fixedEndian, length);
}

inline void serialize(const char &value, std::ostream &output)
{
	writePrimitive(output, value);
}

inline void serialize(const std::int8_t &value, std::ostream &output)
{
	writePrimitive(output, value);
}

inline void serialize(const std::int16_t &value, std::ostream &output)
{
	writePrimitive(output, value);
}

inline void serialize(const std::int32_t &value, std::ostream &output)
{
	writePrimitive(output, value);
}

inline void serialize(const std::int64_t &value, std::ostream &output)
{
	writePrimitive(output, value);
}

inline void serialize(const float &value, std::ostream &output)
{
	writePrimitive(output, value);
}

inline void serialize(const double &value, std::ostream &output)
{
	writePrimitive(output, value);
}

template<typename T>
inline void serialize(const std::shared_ptr<T> &value, std::ostream &output)
{
	return value->serialize(output);
}

template<typename T>
inline void serializeMaybe(std::shared_ptr<T> &val, Output &output)
{
	writePrimitive<std::int8_t>(output, !!val);
	if(val)
		val->serialize(output);
}

template<typename T>
inline void serialize(const std::vector<T> &values, std::ostream &output)
{
	const std::int64_t size = values.size();
	serialize(size, output);
	for(int i = 0; i < size; i++)
	{
		serialize(values[i], output);
	}
}

inline void serialize(const std::string &value, std::ostream &output)
{
	const std::int64_t size = value.size();
	serialize(size, output);
	for(int i = 0; i < size; i++)
	{
		serialize(std::int8_t(value[i]), output);
	}
}

template<typename T>
inline void serialize(const boost::optional<T> &value, std::ostream &output)
{
	const std::int8_t nonempty = value.is_initialized();
	serialize(nonempty, output);

	if(value)
	{
		serialize(*value, output);
	}
}
