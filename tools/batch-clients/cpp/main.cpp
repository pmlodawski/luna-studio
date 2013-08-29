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
        vector<Project> projects;
        batch.projects(projects);

        Project proj = projects[0];


        /* Libraries */

        vector<Library> registeredLibs;

        Library lib1;
        lib1.__set_name("lib1");
        lib1.__set_path("dummylibs/my/lib1.lunalib");

        Library userlib;
        userlib.__set_name("user");
        userlib.__set_path("dummylibs/my/user.lunalib");

        batch.libraries(registeredLibs, proj.projectID);
        cout << "Libraries loaded: " << registeredLibs.size() << endl;

        batch.createLibrary(lib1, lib1, proj.projectID);
        batch.createLibrary(userlib, userlib, proj.projectID);
        
        batch.libraries(registeredLibs, proj.projectID);
        cout << "Libraries loaded: " << registeredLibs.size() << endl;

        /* Add new definition */
        
        Type funInputsType;
        batch.newTypeTuple(funInputsType, {});
        Type funOutputsType;
        batch.newTypeTuple(funOutputsType, {});

        Type funType;
        batch.newTypeFunction(funType, "fun", funInputsType, funOutputsType);

        Definition myModule;
        batch.libraryRootDef(myModule, userlib.libID, proj.projectID);

        cout << myModule.defID << endl;
        Definition fun;
        fun.__set_cls(funType);
        batch.addDefinition(fun, fun, myModule.defID, userlib.libID, proj.projectID);
        batch.updateDefinition(fun, userlib.libID, proj.projectID);
        batch.removeDefinition(fun.defID, userlib.libID, proj.projectID);
        batch.addDefinition(fun, fun, myModule.defID, userlib.libID, proj.projectID);

        Type myclassType;
        batch.newTypeClass(myclassType, "myclass", {}, {});
        
        Definition myclass;
        myclass.__set_cls(myclassType);

        batch.addDefinition(myclass, myclass, myModule.defID, userlib.libID, proj.projectID);

        vector<Definition> children;
        batch.definitionChildren(children, myModule.defID, userlib.libID, proj.projectID);
        cout << "`my` module has " << children.size() << " children." << endl;

        Definition parent;
        batch.definitionParent(parent, fun.defID, userlib.libID, proj.projectID);


        DefsGraph defsGraph;
        batch.defsGraph(defsGraph, userlib.libID, proj.projectID);
        cout << "userlib DefsGraph has " 
             << defsGraph.defs.size() << " defs and " 
             << defsGraph.edges.size() << " edges" << endl;

        batch.defsGraph(defsGraph, registeredLibs[0].libID, proj.projectID);
        cout << "stdlib DefsGraph has " 
             << defsGraph.defs.size() << " defs and " 
             << defsGraph.edges.size() << " edges" << endl;

        /* Add some nodes */

        GraphView graph;
        batch.nodesGraph(graph, fun.defID, userlib.libID, proj.projectID);

        int inputsID = 0;
        int outputsID = 1;

        Node dummy;
        dummy.__set_cls(NodeType::Expr);
        dummy.__set_expression("dummy");
        batch.addNode(dummy, dummy, fun.defID, userlib.libID, proj.projectID);
        dummy.__set_expression("fun");
        batch.updateNode(dummy, fun.defID, userlib.libID, proj.projectID);
        batch.removeNode(dummy.nodeID, fun.defID, userlib.libID, proj.projectID);

        Node dummy2;
        dummy2.__set_cls(NodeType::Expr);
        dummy2.__set_expression("dummy2");
        batch.addNode(dummy2, dummy2, fun.defID, userlib.libID, proj.projectID);
        
        try {
            batch.connect(inputsID, {0, 0, 0}, dummy2.nodeID, {4,5,9}, fun.defID, userlib.libID, proj.projectID);
        } catch (ArgumentException e) {
            cout << "Unable to connect: "<< "\t" << e.message << endl;
        }
        
        batch.connect(inputsID, {6, 9}, dummy2.nodeID, {}, fun.defID, userlib.libID, proj.projectID);
    
        try {
            batch.connect(inputsID, {0, 0, 0}, dummy2.nodeID, {1}, fun.defID, userlib.libID, proj.projectID);
        } catch (ArgumentException e) {
            cout << "Unable to connect: "<< "\t" << e.message << endl;
        }

        batch.connect(inputsID, {1, 2, 5}, outputsID, {1}, fun.defID, userlib.libID, proj.projectID);
        batch.connect(inputsID, {7, 8}, outputsID, {5}, fun.defID, userlib.libID, proj.projectID);

        try {
            batch.connect(inputsID, {0, 0, 0}, outputsID, {}, fun.defID, userlib.libID, proj.projectID);
        } catch (ArgumentException e) {
            cout << "Unable to connect: "<< "\t" << e.message << endl;
        }

        batch.disconnect(inputsID, {1, 2, 5}, outputsID, {5}, fun.defID, userlib.libID, proj.projectID);
        batch.nodesGraph(graph, fun.defID, userlib.libID, proj.projectID);
        
        /* Default values */

        DefaultValue dv;
        dv.__set_cls(DefaultValueType::StringV);
        dv.__set_s("ala");


        DefaultValue dv2;
        dv2.__set_cls(DefaultValueType::IntV);
        dv2.__set_i(4);

        map<PortDescriptor, DefaultValue> defaults;
        batch.nodeDefaults(defaults, outputsID, fun.defID, userlib.libID, proj.projectID);
        cout << "Node has " << defaults.size() << " defaults" << endl;
        batch.setNodeDefault({1}, dv, outputsID, fun.defID, userlib.libID, proj.projectID);
        batch.setNodeDefault({2}, dv2, outputsID, fun.defID, userlib.libID, proj.projectID);
        batch.setNodeDefault({3}, dv2, outputsID, fun.defID, userlib.libID, proj.projectID);
        batch.nodeDefaults(defaults, outputsID, fun.defID, userlib.libID, proj.projectID);
        cout << "Node has " << defaults.size() << " defaults" << endl;
        batch.removeNodeDefault({2}, outputsID, fun.defID, userlib.libID, proj.projectID);
        batch.nodeDefaults(defaults, outputsID, fun.defID, userlib.libID, proj.projectID);
        cout << "Node has " << defaults.size() << " defaults" << endl;
        
        batch.dump();

        /* Loading and unloading */

        batch.storeLibrary(userlib.libID, proj.projectID);
        
        // TODO [PM] Compatibility after removal of Call, Type and New
        // batch.buildLibrary(userlib.libID, proj.projectID);

        batch.libraries(registeredLibs, proj.projectID);
        cout << "Libraries loaded: " << registeredLibs.size() << endl;
        batch.unloadLibrary(userlib.libID, proj.projectID);
        batch.libraries(registeredLibs, proj.projectID);
        cout << "Libraries loaded: " << registeredLibs.size() << endl;
        batch.loadLibrary(userlib, userlib.path, proj.projectID);
        batch.libraries(registeredLibs, proj.projectID);
        cout << "Libraries loaded: " << registeredLibs.size() << endl;

        batch.ping();

        for(auto lib : registeredLibs)
            batch.storeLibrary(lib.libID, proj.projectID);

        batch.storeProject(proj.projectID);
        batch.closeProject(proj.projectID);

        Project reopened;
        batch.openProject(reopened, proj.path);

        for(auto lib : registeredLibs)
            batch.loadLibrary(lib, lib.path, reopened.projectID);
        
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

