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
using namespace flowbox::batch::fs;
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
        lib1.__set_path("dummylibs/my/lib1.lunalib");

        Library userlib;
        userlib.__set_name("user");
        userlib.__set_path("dummylibs/my/user.lunalib");

        batch.libraries(registeredLibs);
        cout << "Libraries loaded: " << registeredLibs.size() << endl;

        batch.createLibrary(lib1, lib1);
        batch.createLibrary(userlib, userlib);
        
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
        batch.libraryRootDef(myModule, userlib.libID);

        cout << myModule.defID << endl;
        Definition fun;
        fun.__set_cls(funType);
        batch.addDefinition(fun, fun, myModule.defID, userlib.libID);
        batch.updateDefinition(fun, userlib.libID);
        batch.removeDefinition(fun.defID, userlib.libID);
        batch.addDefinition(fun, fun, myModule.defID, userlib.libID);

        Type myclassType;
        batch.newTypeClass(myclassType, "myclass", {}, {});
        
        Definition myclass;
        myclass.__set_cls(myclassType);

        batch.addDefinition(myclass, myclass, myModule.defID, userlib.libID);

        vector<Definition> children;
        batch.definitionChildren(children, myModule.defID, userlib.libID);
        cout << "`my` module has " << children.size() << " children." << endl;

        Definition parent;
        batch.definitionParent(parent, fun.defID, userlib.libID);


        DefsGraph defsGraph;
        batch.defsGraph(defsGraph, userlib.libID);
        cout << "userlib DefsGraph has " 
             << defsGraph.defs.size() << " defs and " 
             << defsGraph.edges.size() << " edges" << endl;

        batch.defsGraph(defsGraph, registeredLibs[0].libID);
        cout << "stdlib DefsGraph has " 
             << defsGraph.defs.size() << " defs and " 
             << defsGraph.edges.size() << " edges" << endl;

        /* Add some nodes */

        GraphView graph;
        batch.nodesGraph(graph, fun.defID, userlib.libID);

        Node inputs;
        inputs.__set_cls(NodeType::Inputs);
        batch.addNode(inputs, inputs, fun.defID, userlib.libID);

        Node outputs;
        outputs.__set_cls(NodeType::Outputs);
        batch.addNode(outputs, outputs, fun.defID, userlib.libID);
        batch.updateNode(outputs, fun.defID, userlib.libID);

        Node dummy;
        dummy.__set_cls(NodeType::Call);
        dummy.__set_name("dummy");
        batch.addNode(dummy, dummy, fun.defID, userlib.libID);
        dummy.__set_name("fun");
        batch.updateNode(dummy, fun.defID, userlib.libID);
        batch.removeNode(dummy.nodeID, fun.defID, userlib.libID);

        Node dummy2;
        dummy2.__set_cls(NodeType::Call);
        dummy2.__set_name("dummy2");
        batch.addNode(dummy2, dummy2, fun.defID, userlib.libID);
        
        batch.connect(inputs.nodeID, {4, 9, 2}, dummy2.nodeID, {}, fun.defID, userlib.libID);
        batch.connect(inputs.nodeID, {1, 2, 5}, outputs.nodeID, {1}, fun.defID, userlib.libID);
        batch.connect(inputs.nodeID, {7, 8}, outputs.nodeID, {5}, fun.defID, userlib.libID);
        batch.disconnect(inputs.nodeID, {1, 2, 5}, outputs.nodeID, {1}, fun.defID, userlib.libID);
        
        batch.storeLibrary(userlib.libID);
        batch.buildLibrary(userlib.libID);

        batch.libraries(registeredLibs);
        cout << "Libraries loaded: " << registeredLibs.size() << endl;
        batch.unloadLibrary(userlib.libID);
        batch.libraries(registeredLibs);
        cout << "Libraries loaded: " << registeredLibs.size() << endl;
        batch.loadLibrary(userlib, userlib.path);
        batch.libraries(registeredLibs);
        cout << "Libraries loaded: " << registeredLibs.size() << endl;

        batch.dump();
        batch.ping();

        for(auto lib : registeredLibs)
            batch.storeLibrary(lib.libID);

        Project proj;
        batch.activeProject(proj);
        batch.storeProject(proj.projectID);
        batch.closeProject(proj.projectID);

        Project reopened;
        batch.openProject(reopened, proj.path);
        batch.setActiveProject(reopened.projectID);

        for(auto lib : registeredLibs)
            batch.loadLibrary(lib, lib.path);
        
        vector<FSItem> items;
        batch.FS_ls(items, proj.path);
        cout << "Folder has " << items.size() << " items." << endl;
        batch.FS_touch("test.file");
        batch.FS_mkdir("testfolder");
        batch.FS_cp("test.file", "test2.file");
        batch.FS_mv("test.file", "testfolder/test.file");
        batch.FS_mv("testfolder", "testfolder2");

        batch.FS_touch("test3.file");
        batch.FS_rm("test3.file");

        batch.FS_mkdir("test3folder");
        batch.FS_rm("test3folder");
        
        batch.dump();        
        batch.ping();

    } catch (ArgumentException e) {
        cout << "Batch returned an error: "<< endl
             << "\t" << e.message << endl;
    }

    /* Finalize */

    transport->close();

    return 0;
}
