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

    defs.NodeDefinition newDefinition(1: types.Type type, 2: attrs.Flags flags, 3: attrs.Attributes attrs)
    defs.NodeDefinition addDefinition(1: defs.NodeDefinition definition, 2: defs.NodeDefinition parent)
    void updateDefinition(1: defs.NodeDefinition definition)
    void removeDefinition(1: defs.NodeDefinition definition)

    list<defs.NodeDefinition> definitionChildren(1: defs.NodeDefinition definition)
         defs.NodeDefinition  definitionParent  (1: defs.NodeDefinition definition)

    /*
     * Types
     */

    types.Type newTypeModule   (1: string name)
    types.Type newTypeClass    (1: string name, 2: types.Type params)
    types.Type newTypeFunction (1: string name, 2: types.Type inputs, 3: types.Type outputs)
    types.Type newTypeUdefined ()
    types.Type newTypeNamed    (1: string name, )
    types.Type newTypeVariable (1: string name, 2: types.Type type)
    types.Type newTypeList     (1: types.Type type)
    types.Type newTypeTuple    (1: list<types.Type> types)

    /*
     * Graph
     */
    
    graph.Graph     graph(1: defs.NodeDefinition definition)

    graph.Node    addNode(1: graph.Node node, 2: defs.NodeDefinition definition)
    void       updateNode(1: graph.Node node, 2: defs.NodeDefinition definition)
    void       removeNode(1: graph.Node node, 2: defs.NodeDefinition definition)

    void    connect(1: graph.Node srcNode, 2: graph.PortDescriptor srcPort,
                    3: graph.Node dstNode, 4: graph.PortDescriptor dstPort, 5: defs.NodeDefinition definition)
    void disconnect(1: graph.Node srcNode, 2: graph.PortDescriptor srcPort,
                    3: graph.Node dstNode, 4: graph.PortDescriptor dstPort, 5: defs.NodeDefinition definition)

    /*
     * Other
     */

    void ping()
}