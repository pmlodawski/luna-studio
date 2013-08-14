#include <iostream>

#include "generated/Batch.h"

#include <transport/TSocket.h>
#include <transport/TBufferTransports.h>
#include <protocol/TBinaryProtocol.h>

using namespace apache::thrift;
using namespace apache::thrift::protocol;
using namespace apache::thrift::transport;

using namespace flowbox::batch;
using namespace flowbox::batch::defs;
using namespace flowbox::batch::graph;
using namespace flowbox::batch::libs;
using namespace flowbox::batch::projects;
using namespace flowbox::batch::types;

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

        Library lib1;
        lib1.__set_name("lib1");
        lib1.__set_path("/opt/luna/lib");

        Library userlib;
        userlib.__set_name("lib2");
        userlib.__set_path("~/luna-projects/myproj");

        batch.loadLibrary(lib1, lib1);
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

        Definition myModule;
        batch.libraryRootDef(myModule, userlib);

        cout << myModule.defID << endl;
        Definition fun;
        fun.__set_cls(funType);
        batch.addDefinition(fun, fun, myModule, userlib);
        batch.updateDefinition(fun, userlib);
        batch.removeDefinition(fun, userlib);
        batch.addDefinition(fun, fun, myModule, userlib);

        Type myclassType;
        batch.newTypeClass(myclassType, "myclass", {}, {});
        
        Definition myclass;
        myclass.__set_cls(myclassType);

        batch.addDefinition(myclass, myclass, myModule, userlib);

        vector<Definition> children;
        batch.definitionChildren(children, myModule, userlib);
        cout << "`my` module has " << children.size() << " children." << endl;

        Definition parent;
        batch.definitionParent(parent, fun, userlib);


        DefsGraph defsGraph;
        batch.defsGraph(defsGraph, userlib);
        cout << "userlib DefsGraph has " 
             << defsGraph.defs.size() << " defs and " 
             << defsGraph.edges.size() << " edges" << endl;

        batch.defsGraph(defsGraph, registeredLibs[0]);
        cout << "stdlib DefsGraph has " 
             << defsGraph.defs.size() << " defs and " 
             << defsGraph.edges.size() << " edges" << endl;

        /* Add some nodes */

        GraphView graph;
        batch.nodesGraph(graph, fun, userlib);

        Node inputs;
        inputs.__set_cls(NodeType::Inputs);
        batch.addNode(inputs, inputs, fun, userlib);

        Node outputs;
        outputs.__set_cls(NodeType::Outputs);
        batch.addNode(outputs, outputs, fun, userlib);
        batch.updateNode(outputs, fun, userlib);

        Node dummy;
        dummy.__set_cls(NodeType::Call);
        dummy.__set_name("dummy");
        batch.addNode(dummy, dummy, fun, userlib);
        dummy.__set_name("fun");
        batch.updateNode(dummy, fun, userlib);
        batch.removeNode(dummy, fun, userlib);
        batch.connect(inputs, {1, 2, 5}, outputs, {1}, fun, userlib);
        batch.connect(inputs, {7, 8}, outputs, {5}, fun, userlib);
        batch.disconnect(inputs, {1, 2, 5}, outputs, {1}, fun, userlib);

        batch.buildLibrary(userlib);

        batch.ping();
    } catch (ArgumentException e) {
        cout << "Batch returned an error: "<< endl
             << "\t" << e.message << endl;
    }

    /* Finalize */

    transport->close();

    return 0;
}
