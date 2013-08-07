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

exception ArgumentException {
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

    libs.Library  createLibrary(1: libs.Library library) throws (1: ArgumentException missingFields)
    libs.Library    loadLibrary(1: libs.Library library) throws (1: ArgumentException missingFields)
    void          unloadLibrary(1: libs.Library library) throws (1: ArgumentException missingFields)
    void           storeLibrary(1: libs.Library library) throws (1: ArgumentException missingFields)

    defs.Definition libraryRootDef(1: libs.Library library) throws (1: ArgumentException missingFields)
    /*
     * Definitions
     */

    defs.Definition newDefinition(1: types.Type type, 2: defs.Imports imports, 
                                  3: attrs.Flags flags, 4: attrs.Attributes attrs)
    defs.Definition addDefinition(1: defs.Definition definition, 2: defs.Definition parent) throws (1: ArgumentException missingFields)
    void      updateDefinition(1: defs.Definition definition) throws (1: ArgumentException missingFields)
    void      removeDefinition(1: defs.Definition definition) throws (1: ArgumentException missingFields)

    list<defs.Definition> definitionChildren(1: defs.Definition definition) throws (1: ArgumentException missingFields)
         defs.Definition  definitionParent  (1: defs.Definition definition) throws (1: ArgumentException missingFields)

    /*
     * Types
     */

    types.Type newTypeModule   (1: string name)                             throws (1: ArgumentException missingFields)
    types.Type newTypeClass    (1: string name, 2: list<string> typeparams, 3: list<types.Type> params) throws (1: ArgumentException missingFields)
    types.Type newTypeFunction (1: string name, 2: types.Type inputs, 3: types.Type outputs) throws (1: ArgumentException missingFields)
    types.Type newTypeUdefined ()
    types.Type newTypeNamed    (1: string name, 2: types.Type type) throws (1: ArgumentException missingFields)
    types.Type newTypeVariable (1: string name) throws (1: ArgumentException missingFields)
    types.Type newTypeList     (1: types.Type type) throws (1: ArgumentException missingFields)
    types.Type newTypeTuple    (1: list<types.Type> types) throws (1: ArgumentException missingFields)

    /*
     * Graph
     */
    
    graph.Graph     graph(1: defs.Definition definition) throws (1: ArgumentException missingFields)

    graph.Node    addNode(1: graph.Node node, 2: defs.Definition definition) throws (1: ArgumentException missingFields)
    void       updateNode(1: graph.Node node, 2: defs.Definition definition) throws (1: ArgumentException missingFields)
    void       removeNode(1: graph.Node node, 2: defs.Definition definition) throws (1: ArgumentException missingFields)

    void    connect(1: graph.Node srcNode, 2: graph.PortDescriptor srcPort,
                    3: graph.Node dstNode, 4: graph.PortDescriptor dstPort, 5: defs.Definition definition) throws (1: ArgumentException missingFields)
    void disconnect(1: graph.Node srcNode, 2: graph.PortDescriptor srcPort,
                    3: graph.Node dstNode, 4: graph.PortDescriptor dstPort, 5: defs.Definition definition) throws (1: ArgumentException missingFields)

    /*
     * Other
     */

    void ping()
}