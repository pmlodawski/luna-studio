#include "gen-cpp/Batch.h"

#include <transport/TSocket.h>
#include <transport/TBufferTransports.h>
#include <protocol/TBinaryProtocol.h>

using namespace apache::thrift;
using namespace apache::thrift::protocol;
using namespace apache::thrift::transport;

using namespace flowbox::batch;
using namespace std;

int main(int argc, char **argv) {

    /* Prepare Batch connection */

    boost::shared_ptr<TSocket> socket(new TSocket("localhost", 30521));
    boost::shared_ptr<TTransport> transport(new TBufferedTransport(socket));
    boost::shared_ptr<TProtocol> protocol(new TBinaryProtocol(transport));

    BatchClient batch(protocol);
    transport->open();
    
    batch.ping();

    /* Libraries */

    vector<Library> registeredLibs;

    Library stdlib;
    stdlib.name = "/opt/luna/lib";
    stdlib.path = "std";

    Library userlib;
    userlib.name = "~/luna-projects/myproj";
    userlib.path = "my";  

    batch.loadLibrary(stdlib);
    batch.loadLibrary(userlib);
    batch.libraries(registeredLibs);
    batch.unloadLibrary(userlib);
    batch.libraries(registeredLibs);
    batch.loadLibrary(userlib);
    batch.libraries(registeredLibs);

    /* Add new definition */
    
    Type funInputsType;
    batch.newTypeTuple(funInputsType, {});
    Type funOutputsType;
    batch.newTypeTuple(funOutputsType, {});

    Type funType;
    batch.newTypeFunction(funType, "fun", funInputsType, funOutputsType);

    NodeDefinition myModule;// TODO [PM] How can I get "my" module?

    NodeDefinition fun;
    fun.cls = funType;
    batch.addDefinition(fun, fun, myModule);

    /* Add some nodes */

    Node inputs;
    inputs.cls = NodeType::Inputs;
    inputs.name = "inputs";
    batch.addNode(inputs, inputs, fun);

    Node outputs;
    outputs.cls = NodeType::Outputs;
    batch.addNode(outputs, outputs, fun);
    outputs.name = "outputs";
    batch.updateNode(outputs, fun);

    Node dummy;
    dummy.cls = NodeType::Call;
    dummy.name = "dummy";
    batch.addNode(dummy, dummy, fun);
    batch.removeNode(dummy, fun);

    /* Finalize */

    transport->close();

    return 0;
}