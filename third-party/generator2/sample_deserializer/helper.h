#pragma once

#include <vector>
#include <memory>
#include <cstdint>
#include <iostream>
#include <string>
#include <tuple>
#include <boost/optional.hpp>

typedef std::istream Input;
typedef std::ostream Output;

struct membuf : std::streambuf
{
	membuf(char const* base, size_t size)
	{
		char* p(const_cast<char*>(base));
		this->setg(p, p, p + size);
	}
};

struct imemstream : virtual membuf, std::istream
{
	imemstream(char const* base, size_t size)
		: membuf(base, size)
		, std::istream(static_cast<std::streambuf*>(this))
	{
		exceptions(std::ios::eofbit | std::ios::badbit | std::ios::failbit);
	};

	imemstream(const std::string *memory1)
		: imemstream(memory1->data(), memory1->size())
	{}
};

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

namespace binary
{
	template<typename T>
	inline void serialize(const boost::optional<T> &value, std::ostream &output);

	template<typename ...Args>
	inline void serialize(const std::tuple<Args...> &value, std::ostream &output);

	template<typename T>
	inline void serialize(const std::vector<T> &values, std::ostream &output);

	template<typename ...Args>
	inline void deserialize(std::tuple<Args...> &value, std::istream &input);

	template<typename T>
	inline void deserialize(boost::optional<T> &out, std::istream &input);

	template<typename T>
	inline void deserialize(std::vector<T> &out, std::istream &input);

	template<typename T>
	inline T readPrimitive(std::istream &input)
	{
		T ret;
		input.read((char*)&ret, sizeof(ret));
		ret = swap_endian(ret);
		return ret;
	}
	
	template<typename T>
	T readFloatingPoint(Input &input)
	{
		return readPrimitive<T>(input);
		// 	T ret = 0;
		// 	double mantissa = readInteger(input);
		// 	double exponent = readInt64(input);
		// 	ret = std::ldexp(mantissa, exponent);
		// 	return ret;
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

	inline bool readBool(std::istream &input)
	{
		return readInt8(input);
	}

	inline float readFloat(std::istream &input)
	{
		return readFloatingPoint<float>(input);
	}

	inline double readDouble(std::istream &input)
	{
		return readFloatingPoint<double>(input);
	}

	inline void deserialize(std::int8_t &val, Input &input);
	template<typename T>
	inline void deserialize(std::vector<T> &out, std::istream &input);

	inline long long readInteger(Input &input)
	{
		auto isBig = readInt8(input);
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
			for(int i = 0; i < (int)bytes.size(); i++)
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
		val = T::deserializeFrom(input);
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
		out.resize((size_t)size);
		for(int i = 0; i < size; i++)
		{
			deserialize(out[i], input);
		}
	}

	inline std::string deserializeUtf8CharacterAsString(Input &input)
	{
		char bytes[5] = {0};
	
		bytes[0] = readInt8(input);
		if((unsigned char)bytes[0] < 0x80)
			return {bytes};

		bytes[1] = readInt8(input);
		if((unsigned char)bytes[0] < 0xE0)
			return{ bytes };

		bytes[2] = readInt8(input);
		if((unsigned char)bytes[0] < 0xF0)
			return{ bytes };

		bytes[3] = readInt8(input);
		return bytes;
	}


	inline void deserialize(std::string &out, std::istream &input)
	{
		auto size = readInt64(input);
		out.resize((size_t)size);
		out.clear();

		for(int i = 0; i < size; i++)
		{
			out += deserializeUtf8CharacterAsString(input);
		}
	}

	template<typename T = float>
	void deserialize(QPointF &pt, std::istream &input)
	{
		T x, y;
		binary::deserialize(x, input);
		binary::deserialize(y, input);
		pt.setX(x);
		pt.setY(y);
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

	inline void writeInt8(std::int8_t f, std::ostream &output)
	{
		writePrimitive(output, f);
	}

	inline void writeInt16(std::int16_t f, std::ostream &output)
	{
		writePrimitive(output, f);
	}

	inline void writeInt32(std::int32_t f, std::ostream &output)
	{
		writePrimitive(output, f);
	}

	inline void writeInt64(std::int64_t f, std::ostream &output)
	{
		writePrimitive(output, f);
	}

	inline void writeFloat(float f, std::ostream &output)
	{
		writePrimitive(output, f);
	}

	inline void serialize(const char &value, std::ostream &output)
	{
		writePrimitive(output, value);
	}

	inline void serialize(const std::int8_t &value, std::ostream &output)
	{
		writePrimitive(output, value);
	}

	inline void serialize(const bool &value, std::ostream &output)
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
	inline void serializeMaybe(const std::shared_ptr<T> &val, Output &output)
	{
		writePrimitive<std::int8_t>(output, !!val);
		if(val)
			val->serialize(output);
	}

	template<typename T = float>
	void serialize(const QPointF &pt, std::ostream &out)
	{
		binary::serialize((T)pt.x(), out);
		binary::serialize((T)pt.y(), out);
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
		// Count UTF-8 characters
		const std::int64_t length = std::count_if(value.begin(), value.end(), [](const unsigned char &c)
		{
			const unsigned char mask = 0xC0; // 11 00 00 00
			const unsigned char test = 0x80; // 10 00 00 00

			return (c & mask) != test;
		});


		serialize(length, output);	
		for(int i = 0; i < (int)value.size(); i++)
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

	// Define a type which holds an unsigned integer value 
	template<std::size_t> struct int_ {};

	template <class Tuple, size_t Pos>
	void serializeTuple(const Tuple& t, std::ostream& out, int_<Pos>)
	{
		const auto &value = std::get<std::tuple_size<Tuple>::value - Pos>(t);
		serialize(value, out);
		serializeTuple(t, out, int_<Pos - 1>());
	}

	template <class Tuple>
	void serializeTuple(const Tuple& t, std::ostream& out, int_<1>)
	{
		const auto &value = std::get<std::tuple_size<Tuple>::value - 1>(t);
		serialize(value, out);
	}

	template <class Tuple, size_t Pos>
	void deserializeTuple(Tuple& t, std::istream& in, int_<Pos>)
	{
		auto &value = std::get<std::tuple_size<Tuple>::value - Pos>(t);
		deserialize(value, in);
		deserializeTuple(t, in, int_<Pos - 1>());
	}

	template <class Tuple>
	void deserializeTuple(Tuple& t, std::istream& in, int_<1>)
	{
		auto &value = std::get<std::tuple_size<Tuple>::value - 1>(t);
		deserialize(value, in);
	}

	inline void serialize(const std::tuple<> &value, std::ostream &output)
	{}

	template<typename ...Args>
	inline void serialize(const std::tuple<Args...> &value, std::ostream &output)
	{
		serializeTuple(value, output, int_<sizeof...(Args)>());
	}

	inline void deserialize(std::tuple<> &value, std::istream &input)
	{}

	template<typename ...Args>
	inline void deserialize(std::tuple<Args...> &value, std::istream &input)
	{
		deserializeTuple(value, input, int_<sizeof...(Args)>());
	}
}
