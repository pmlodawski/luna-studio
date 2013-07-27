#include "gen-cpp/Batch.h"

#include <transport/TSocket.h>
#include <transport/TBufferTransports.h>
#include <protocol/TBinaryProtocol.h>

using namespace apache::thrift;
using namespace apache::thrift::protocol;
using namespace apache::thrift::transport;

// using namespace Test;

int main(int argc, char **argv) {
    boost::shared_ptr<TSocket> socket(new TSocket("localhost", 30521));
    boost::shared_ptr<TTransport> transport(new TBufferedTransport(socket));
    boost::shared_ptr<TProtocol> protocol(new TBinaryProtocol(transport));

    BatchClient client(protocol);
    transport->open();
    for(int i = 0; i < 5 ; ++i)
        client.ping();
    transport->close();

    return 0;
}