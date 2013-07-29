#include "gen-cpp/Batch.h"

#include <transport/TSocket.h>
#include <transport/TBufferTransports.h>
#include <protocol/TBinaryProtocol.h>

using namespace apache::thrift;
using namespace apache::thrift::protocol;
using namespace apache::thrift::transport;

using namespace flowbox::batch;


int main(int argc, char **argv) {
    boost::shared_ptr<TSocket> socket(new TSocket("localhost", 30521));
    boost::shared_ptr<TTransport> transport(new TBufferedTransport(socket));
    boost::shared_ptr<TProtocol> protocol(new TBinaryProtocol(transport));

    BatchClient batch(protocol);
    transport->open();
    

    /* Libraries */

    Library stdlib;
    stdlib.name = "/opt/luna/lib";
    stdlib.path = "std";

    Library userlib;
    userlib.name = "~/luna-projects/myproj";
    userlib.path = "my";  

    batch.loadLibrary(stdlib);
    batch.loadLibrary(userlib);
    batch.unloadLibrary(userlib);
        
    for(int i = 0; i < 5 ; ++i)
        batch.ping();

    transport->close();

    return 0;
}