
#ifdef _WIN32
    #pragma comment(lib, "libprotobuf.lib")
    #define _WIN32_WINNT 0x501 //Needs to come before asio.hpp
#endif


#include <chrono>
#include <iostream>
#include <string>
#include <algorithm>
#include <type_traits>
#include <cstdint>

#include <google/protobuf/io/zero_copy_stream_impl.h>
#include <google/protobuf/io/coded_stream.h>

#include "generated/server-api.pb.h"  

#include <boost/asio.hpp>



using boost::asio::ip::tcp;
using Socket = tcp::socket;

using namespace google::protobuf::io;

using namespace generated::proto;
using namespace generated::proto::batch;

const int BUFFER_SIZE = 10000000; // TODO [PM] : magic constant
char buffer[BUFFER_SIZE];

const std::string host = "localhost", controlPort = "30521", notifyPort = "30522";

class StopWatch
{
    typedef std::chrono::high_resolution_clock clock;
    typedef std::chrono::microseconds microseconds;
    typedef std::chrono::milliseconds milliseconds;

    milliseconds intervalMs(const clock::time_point& t1, const clock::time_point& t0)
    {
        return std::chrono::duration_cast<milliseconds>(t1 - t0);
    }

    clock::time_point start_;
public:
    StopWatch() : start_(clock::now()) {}
    clock::time_point restart() { start_ = clock::now(); return start_; }
    milliseconds elapsedMs()
    {
        return std::chrono::duration_cast<milliseconds>(clock::now() - start_);
    }
};

size_t sendAll(Socket &controlSocket, void *data, size_t size)
{
    size_t sent = boost::asio::write(controlSocket, boost::asio::buffer(data, size));
    assert(sent == size);
    return sent;
}


void sendRequest(Socket &controlSocket, const Request& request)
{
    int  requestSize = request.ByteSize() + 4;
    char* requestBuf = new char[requestSize];

    //write varint delimiter to buffer
    ArrayOutputStream arrayOut(requestBuf, requestSize);
    CodedOutputStream codedOut(&arrayOut);
    codedOut.WriteVarint32(request.ByteSize());

    //write protobuf ack to buffer
    request.SerializeToCodedStream(&codedOut);

    //send buffer to client
    sendAll(controlSocket, requestBuf, requestSize);
    // std::cout << "Sent: " << sent << std::flush;

    delete [] requestBuf;
}

Response receiveResponse(Socket &controlSocket)
{
    Response response;
    size_t received = boost::asio::read(controlSocket, boost::asio::buffer(buffer, 3));

    //read varint delimited protobuf object in to buffer
    //there's no method to do this in the C++ library so here's the workaround
    ArrayInputStream headerArrayIn(buffer, received);
    CodedInputStream headerCodedIn(&headerArrayIn);
    google::protobuf::uint32 packetSize;
    headerCodedIn.ReadVarint32(&packetSize);
    const int sizeinfoLength = headerCodedIn.CurrentPosition();
    const int remainingToRead = packetSize + sizeinfoLength - received;
    received = boost::asio::read(controlSocket, boost::asio::buffer(buffer + received, remainingToRead));

    ArrayInputStream arrayIn(buffer + sizeinfoLength, packetSize);
    CodedInputStream codedIn(&arrayIn);
    CodedInputStream::Limit msgLimit = codedIn.PushLimit(packetSize);
    response.ParseFromCodedStream(&codedIn);
    codedIn.PopLimit(msgLimit);
    return response;
}

Response call(Socket &controlSocket, const Request& request)
{
    sendRequest(controlSocket, request);
    return receiveResponse(controlSocket);
}

Response callAndTranslateException(Socket &controlSocket, const Request& request)
{
    auto response = call(controlSocket, request);

    if(response.type() == generated::proto::batch::Response_Type_Exception)
    {
        const auto exc = response.GetExtension(Exception::rsp);
        const auto msg = exc.message();
        throw std::runtime_error(msg);
    }

    return response;
}

typedef std::vector<std::pair<generated::proto::crumb::Crumb_Cls, std::string>> BreadcrumbsHelper;
generated::proto::crumb::Breadcrumbs *buildBreadcrumbs(const BreadcrumbsHelper &breadcrumbsInfo)
{
    auto breadcrumbs = new generated::proto::crumb::Breadcrumbs();
    for(auto &crumbDescriptor : breadcrumbsInfo)
    {
        auto crumb = breadcrumbs->mutable_crumbs()->Add();
        crumb->set_cls(crumbDescriptor.first);
        crumb->set_name(crumbDescriptor.second);
    }
    return breadcrumbs;
}

template <typename TMessage>
void setField(TMessage *msg, int i, int value)
{
    msg->GetReflection()->SetInt32(msg, TMessage::descriptor()->field(i), value);
}

template <typename TMessage>
void setField(TMessage *msg, int i, const char* value)
{
    msg->GetReflection()->SetString(msg, TMessage::descriptor()->field(i), value);
}

template <typename TMessage>
void setField(TMessage *msg, int i, std::string value)
{
     msg->GetReflection()->SetString(msg, TMessage::descriptor()->field(i), value);
}

template <typename TMessage>
void setField(TMessage *msg, int i, std::vector<int> value)
{
    for(size_t index = 0 ; index < value.size() ; ++index)
        msg->GetReflection()->AddInt32(msg, TMessage::descriptor()->field(i), value[index]);
}

template <typename TMessage, typename SomeMessageType>
void setField(TMessage *msg, int i, SomeMessageType *value)
{
    static_assert(std::is_base_of<google::protobuf::Message, SomeMessageType>::value, "Cannot set field to a non-message pointer");
    auto messageField = msg->GetReflection()->MutableMessage(msg, TMessage::descriptor()->field(i));
    messageField->GetReflection()->Swap(messageField, value);
    delete value;
}

template <typename TMessage, typename SomeMessageType>
void setField(TMessage *msg, int i, SomeMessageType value)
{
    static_assert(std::is_base_of<google::protobuf::Message, SomeMessageType>::value, "Cannot set field to a non-message pointer");
    auto messageField = msg->GetReflection()->MutableMessage(msg, TMessage::descriptor()->field(i));
    messageField->GetReflection()->Swap(messageField, &value);
}

/*
template <typename TMessage, typename TField>
void setField(TMessage *msg, int, const TField &)
{
    static_assert(0, "Don't know how to set this kind of value");
}*/

template <int index, typename TMessage>
void setArgsInternal(TMessage *ms)
{
}

template <int index, typename TMessage, typename Arg, typename ... Args>
void setArgsInternal(TMessage *msg, Arg arg1, Args ... args)
{
    setField(msg, index, arg1);
    setArgsInternal<index + 1>(msg, args...);
}

template <typename TMessage, typename ... Args>
TMessage *buildArgs(Args ... args)
{
    auto ret = new TMessage(); 
    setArgsInternal<0, TMessage, Args...>(ret, args...);
    return ret;
}

template<typename TResult, typename TArgs>
TResult askSecondaryWrapper(tcp::socket &controlSocket, TArgs * args, 
                             generated::proto::batch::Request_Method method)
{
    Request request;
    request.set_method(method);
    request.SetAllocatedExtension(TArgs::req, args);

    auto response = callAndTranslateException(controlSocket, request);
    assert(response.type() == Response_Type_Result || response.type() == Response_Type_Accept); //exception would be translated to exception
    auto defaultsResponse = response.GetExtension(TResult::rsp);
    response.Clear();
    return defaultsResponse;
}

#define makeAsk(space, method) namespace macro { namespace space {              \
space ##_ ## method ## _Result                                                  \
method##_(tcp::socket &controlSocket, space ## _ ## method ## _Args *args)             \
{                                                                               \
    return askSecondaryWrapper                                                  \
    <space ##_ ## method ## _Result, space ## _ ## method ## _Args>             \
    (controlSocket, args, Request_Method_ ## space ## _ ## method);                    \
}                                                                               \
                                                                                \
template<typename ...Args>                                                      \
space ##_ ## method ## _Result                                                  \
method(tcp::socket &controlSocket, const Args & ...args)                               \
{                                                                               \
    return method##_(controlSocket, buildArgs<space ## _ ## method ## _Args>(args...));\
} } }


makeAsk(Maintenance, Dump)


int main()
{
    try
    {
        //Prepare Boost.Asio
        boost::asio::io_service io_service;
        tcp::socket controlSocket(io_service);
        tcp::resolver resolver(io_service);

        tcp::socket notifySocket(io_service);


        //Connect
        std::cout << "Connecting to " << host << " at port " << controlPort << " / " << notifyPort << std::endl;
        boost::asio::connect(controlSocket, resolver.resolve({ host, controlPort }));
        boost::asio::connect(notifySocket, resolver.resolve({ host, notifyPort }));

        macro::Maintenance::Dump(controlSocket);

        return EXIT_SUCCESS;
    }
    catch(std::exception &e)
    {
        std::cout << "Encountered an exception: " << e.what() << std::endl;
        return EXIT_FAILURE;
    }
}
