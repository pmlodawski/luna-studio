#include <iostream>

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

    try {
        /* Libraries */

        vector<Library> registeredLibs;

        Library stdlib;
        stdlib.__set_name("std");
        stdlib.__set_path("/opt/luna/lib");

        Library userlib;
        userlib.__set_name("my");
        userlib.__set_path("~/luna-projects/myproj");

        batch.loadLibrary(stdlib, stdlib);
        batch.loadLibrary(userlib, userlib);
        batch.libraries(registeredLibs);
        cout << "Libraries loaded: " << registeredLibs.size() << endl;
        batch.unloadLibrary(userlib);
        batch.libraries(registeredLibs);
        cout << "Libraries loaded: " << registeredLibs.size() << endl;
        batch.loadLibrary(userlib, userlib);

        try {
            Library brokenLib;
            brokenLib.__set_name("brokenLib");
            batch.loadLibrary(brokenLib, brokenLib);
        } catch (ArgumentException e) {
            cout << e.message << endl;
        }

        batch.libraries(registeredLibs);
        cout << "Libraries loaded: " << registeredLibs.size() << endl;



        /* Add new definition */
        
        Type funInputsType;
        batch.newTypeTuple(funInputsType, {});
        Type funOutputsType;
        batch.newTypeTuple(funOutputsType, {});

        Type funType;
        batch.newTypeFunction(funType, "fun", funInputsType, funOutputsType);

        NodeDef myModule;
        batch.libraryRootDef(myModule, userlib);

        cout << myModule.defID << endl;
        NodeDef fun;
        fun.__set_cls(funType);
        
        batch.addDefinition(fun, fun, myModule);
        batch.updateDefinition(fun);
        batch.removeDefinition(fun);
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

    } catch (ArgumentException e) {
        cout << "Batch returned an error: "<< endl
             << "\t" << e.message << endl;
    }

    /* Finalize */

    transport->close();

    return 0;
}