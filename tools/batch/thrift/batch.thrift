///////////////////////////////////////////////////////////////////////////
// Copyright (C) Flowbox, Inc / All Rights Reserved
// Unauthorized copying of this file, via any medium is strictly prohibited
// Proprietary and confidential
// Flowbox Team <contact@flowbox.io>, 2013
///////////////////////////////////////////////////////////////////////////

namespace cpp flowbox.batch
namespace hs  flowbox.batch

include "../../lunac/thrift/attrs.thrift"
include "../../lunac/thrift/defs.thrift"
include "../../lunac/thrift/graph.thrift"
include "../../lunac/thrift/libs.thrift"
include "../../lunac/thrift/types.thrift"

/*
 * Exceptions
 */

exception MissingFieldsException {
    1: optional string message;
}

/*
 * Batch service
 */

service Batch {

    /*
     * Libraries
     */

    list<libs.Library> libraries()

    libs.Library   loadLibrary(1: libs.Library library) throws (1:MissingFieldsException missingFields)
    void         unloadLibrary(1: libs.Library library) throws (1:MissingFieldsException missingFields)

    /*
     * Definitions
     */

    defs.NodeDefinition newDefinition(1: types.Type type, 2: defs.Imports imports, 
                                      3: attrs.Flags flags, 4: attrs.Attributes attrs)
    defs.NodeDefinition addDefinition(1: defs.NodeDefinition definition, 2: defs.NodeDefinition parent) throws (1:MissingFieldsException missingFields)
    void updateDefinition(1: defs.NodeDefinition definition) throws (1:MissingFieldsException missingFields)
    void removeDefinition(1: defs.NodeDefinition definition) throws (1:MissingFieldsException missingFields)

    list<defs.NodeDefinition> definitionChildren(1: defs.NodeDefinition definition) throws (1:MissingFieldsException missingFields)
         defs.NodeDefinition  definitionParent  (1: defs.NodeDefinition definition) throws (1:MissingFieldsException missingFields)

    /*
     * Types
     */

    types.Type newTypeModule   (1: string name) throws (1:MissingFieldsException missingFields)
    types.Type newTypeClass    (1: string name, 2: types.Type params) throws (1:MissingFieldsException missingFields)
    types.Type newTypeFunction (1: string name, 2: types.Type inputs, 3: types.Type outputs) throws (1:MissingFieldsException missingFields)
    types.Type newTypeUdefined () throws (1:MissingFieldsException missingFields)
    types.Type newTypeNamed    (1: string name, ) throws (1:MissingFieldsException missingFields)
    types.Type newTypeVariable (1: string name, 2: types.Type type) throws (1:MissingFieldsException missingFields)
    types.Type newTypeList     (1: types.Type type) throws (1:MissingFieldsException missingFields)
    types.Type newTypeTuple    (1: list<types.Type> types) throws (1:MissingFieldsException missingFields)

    /*
     * Graph
     */
    
    graph.Graph     graph(1: defs.NodeDefinition definition) throws (1:MissingFieldsException missingFields)

    graph.Node    addNode(1: graph.Node node, 2: defs.NodeDefinition definition) throws (1:MissingFieldsException missingFields)
    void       updateNode(1: graph.Node node, 2: defs.NodeDefinition definition) throws (1:MissingFieldsException missingFields)
    void       removeNode(1: graph.Node node, 2: defs.NodeDefinition definition) throws (1:MissingFieldsException missingFields)

    void    connect(1: graph.Node srcNode, 2: graph.PortDescriptor srcPort,
                    3: graph.Node dstNode, 4: graph.PortDescriptor dstPort, 5: defs.NodeDefinition definition) throws (1:MissingFieldsException missingFields)
    void disconnect(1: graph.Node srcNode, 2: graph.PortDescriptor srcPort,
                    3: graph.Node dstNode, 4: graph.PortDescriptor dstPort, 5: defs.NodeDefinition definition) throws (1:MissingFieldsException missingFields)

    /*
     * Other
     */

    void ping()
}