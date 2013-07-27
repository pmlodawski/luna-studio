///////////////////////////////////////////////////////////////////////////
// Copyright (C) Flowbox, Inc / All Rights Reserved
// Unauthorized copying of this file, via any medium is strictly prohibited
// Proprietary and confidential
// Flowbox Team <contact@flowbox.io>, 2013
///////////////////////////////////////////////////////////////////////////

//namespace cpp flowbox.batch
//namespace hs  flowbox.batch

include "attrs.thrift"
include "defs.thrift"
include "graph.thrift"
include "libs.thrift"
include "types.thrift"


service Batch {

    /*
     * Libraries
     */

    list<libs.Library> libraries()

    void   registerLibrary(1: libs.Library library)
    void     updateLibrary(1: libs.Library library)
    void unregisterLibrary(1: libs.Library library)

    /*
     * Definitions
     */

    void    addDefinition(1: defs.NodeDefinition definition, 2: defs.NodeDefinition parent, 3: libs.Library library)
    void updateDefinition(1: defs.NodeDefinition definition)
    void removeDefinition(1: defs.NodeDefinition definition)

    list<defs.NodeDefinition> definitionChildren(1: defs.NodeDefinition definition)
         defs.NodeDefinition  definitionParent  (1: defs.NodeDefinition definition)

    /*
     * Graph
     */
    
    list<graph.Node> nodes(1: defs.NodeDefinition definition)

    void    addNode(1: graph.Node node, 2: defs.NodeDefinition definition)
    void updateNode(1: graph.Node node, 2: defs.NodeDefinition definition)
    void removeNode(1: graph.Node node, 2: defs.NodeDefinition definition)

    void    connect(1: graph.Node srcNode, 2: graph.PortDescriptor srcPort,
                    3: graph.Node dstNode, 4: graph.PortDescriptor dstPort, 5: defs.NodeDefinition definition)
    void disconnect(1: graph.Node srcNode, 2: graph.PortDescriptor srcPort,
                    3: graph.Node dstNode, 4: graph.PortDescriptor dstPort, 5: defs.NodeDefinition definition)

    /*
     * Other
     */

    void ping()
}