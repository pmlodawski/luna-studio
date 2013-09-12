///////////////////////////////////////////////////////////////////////////
// Copyright (C) Flowbox, Inc / All Rights Reserved
// Unauthorized copying of this file, via any medium is strictly prohibited
// Proprietary and confidential
// Flowbox Team <contact@flowbox.io>, 2013
///////////////////////////////////////////////////////////////////////////

namespace cpp flowbox.batch
namespace hs  flowbox.batch

include "../../../../../../../libs/batch/src/Flowbox/Batch/Tools/Serialize/Thrift/fs.thrift"
include "../../../../../../../libs/batch/src/Flowbox/Batch/Tools/Serialize/Thrift/graphview.thrift"
include "../../../../../../../libs/batch/src/Flowbox/Batch/Tools/Serialize/Thrift/projects.thrift"
include "../../../../../../../libs/luna/src/Flowbox/Luna/Tools/Serialize/Thrift/attrs.thrift"
include "../../../../../../../libs/luna/src/Flowbox/Luna/Tools/Serialize/Thrift/defs.thrift"
include "../../../../../../../libs/luna/src/Flowbox/Luna/Tools/Serialize/Thrift/graph.thrift"
include "../../../../../../../libs/luna/src/Flowbox/Luna/Tools/Serialize/Thrift/libs.thrift"
include "../../../../../../../libs/luna/src/Flowbox/Luna/Tools/Serialize/Thrift/types.thrift"

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
     * Projects
     */

    list<projects.Project> projects()

    projects.Project    projectByID(1: projects.ProjectID projectID) throws (1: ArgumentException missingFields)
    projects.Project  createProject(1: projects.Project   project  ) throws (1: ArgumentException missingFields)
    projects.Project    openProject(1: string             path     ) throws (1: ArgumentException missingFields)
    void              updateProject(1: projects.Project   project  ) throws (1: ArgumentException missingFields)
    void               closeProject(1: projects.ProjectID projectID) throws (1: ArgumentException missingFields)
    void               storeProject(1: projects.ProjectID projectID) throws (1: ArgumentException missingFields)

    /*
     * Libraries
     */

    list<libs.Library>   libraries(1: projects.ProjectID projectID) throws (1: ArgumentException missingFields)

    libs.Library       libraryByID(1: libs.LibID        libraryID, 
                                   2: projects.ProjectID projectID) throws (1: ArgumentException missingFields)

    libs.Library     createLibrary(1: libs.Library       library, 
                                   2: projects.ProjectID projectID) throws (1: ArgumentException missingFields)
    
    libs.Library       loadLibrary(1: string             path, 
                                   2: projects.ProjectID projectID) throws (1: ArgumentException missingFields)
    
    void             unloadLibrary(1: libs.LibID         libID, 
                                   2: projects.ProjectID projectID) throws (1: ArgumentException missingFields)
    
    void              storeLibrary(1: libs.LibID         libID, 
                                   2: projects.ProjectID projectID) throws (1: ArgumentException missingFields)
    
    void              buildLibrary(1: libs.LibID         libID,
                                   2: projects.ProjectID projectID) throws (1: ArgumentException missingFields)

    defs.Definition libraryRootDef(1: libs.LibID         libID,
                                   2: projects.ProjectID projectID) throws (1: ArgumentException missingFields)
  
    /*
     * Definitions
     */

    defs.DefsGraph      defsGraph(1: libs.LibID         libID, 
                                  2: projects.ProjectID projectID) throws (1: ArgumentException missingFields)

    defs.Definition       defByID(1: defs.DefID         defID,
                                  2: libs.LibID         libID,
                                  3: projects.ProjectID projectID) throws (1: ArgumentException missingFields)

    defs.Definition addDefinition(1: defs.Definition    definition, 
                                  2: defs.DefID         parentID,
                                  3: libs.LibID         libID,
                                  4: projects.ProjectID projectID) throws (1: ArgumentException missingFields)

    void         updateDefinition(1: defs.Definition    definition, 
                                  2: libs.LibID         libID, 
                                  3: projects.ProjectID projectID) throws (1: ArgumentException missingFields)

    void         removeDefinition(1: defs.DefID         defID,
                                  2: libs.LibID         libID, 
                                  3: projects.ProjectID projectID) throws (1: ArgumentException missingFields)

    list<defs.Definition> definitionChildren(1: defs.DefID         defID, 
                                             2: libs.LibID         libID,
                                             3: projects.ProjectID projectID) throws (1: ArgumentException missingFields)

    defs.Definition         definitionParent(1: defs.DefID         defID, 
                                             2: libs.LibID         libID,
                                             3: projects.ProjectID projectID) throws (1: ArgumentException missingFields)

    list<defs.DefPtr>      resolveDefinition(1: string             name,
                                             2: defs.DefID         parentID, 
                                             3: libs.LibID         libID,
                                             4: projects.ProjectID projectID) throws (1: ArgumentException missingFields)
    /*
     * Types
     */

    types.Type newTypeModule   (1: string           name,
                                2: list<types.Type> fields)     throws (1: ArgumentException missingFields)
    types.Type newTypeClass    (1: string           name, 
                                2: list<string>     params,
                                3: list<types.Type> fields)     throws (1: ArgumentException missingFields)
    types.Type newTypeFunction (1: string           name, 
                                2: types.Type       inputs, 
                                3: types.Type       outputs)    throws (1: ArgumentException missingFields)
    types.Type newTypeUdefined ()
    types.Type newTypeNamed    (1: string           name, 
                                2: types.Type       type)       throws (1: ArgumentException missingFields)
    types.Type newTypeName     (1: string           name)       throws (1: ArgumentException missingFields)
    types.Type newTypeTuple    (1: list<types.Type> types)      throws (1: ArgumentException missingFields)

    /*
     * Graph
     */
    
    graphview.GraphView 
               nodesGraph(1: defs.DefID         defID, 
                          2: libs.LibID         libID,
                          3: projects.ProjectID projectID) throws (1: ArgumentException missingFields)

    graph.Node   nodeByID(1: graph.NodeID       nodeID,
                          2: defs.DefID         defID, 
                          3: libs.LibID         libID,
                          4: projects.ProjectID projectID) throws (1: ArgumentException missingFields)

    graph.Node    addNode(1: graph.Node         node, 
                          2: defs.DefID         defID, 
                          3: libs.LibID         libID,
                          4: projects.ProjectID projectID) throws (1: ArgumentException missingFields)

    void       updateNode(1: graph.Node         node, 
                          2: defs.DefID         defID, 
                          3: libs.LibID         libID,
                          4: projects.ProjectID projectID) throws (1: ArgumentException missingFields)

    void       removeNode(1: graph.NodeID       nodeID,
                          2: defs.DefID         defID, 
                          3: libs.LibID         libID,
                          4: projects.ProjectID projectID) throws (1: ArgumentException missingFields)

    void          connect(1: graph.NodeID             srcNodeID, 
                          2: graphview.PortDescriptor srcPort,
                          3: graph.NodeID             dstNodeID, 
                          4: graphview.PortDescriptor dstPort, 
                          5: defs.DefID               defID, 
                          6: libs.LibID               libID,
                          7: projects.ProjectID       projectID) throws (1: ArgumentException missingFields)

    void       disconnect(1: graph.NodeID             srcNodeID, 
                          2: graphview.PortDescriptor srcPort,
                          3: graph.NodeID             dstNodeID, 
                          4: graphview.PortDescriptor dstPort, 
                          5: defs.DefID               defID, 
                          6: libs.LibID               libID,
                          7: projects.ProjectID       projectID) throws (1: ArgumentException missingFields)

    map<graphview.PortDescriptor, graph.DefaultValue> 
              nodeDefaults(1: graph.NodeID       nodeID,
                           2: defs.DefID         defID, 
                           3: libs.LibID         libID,
                           4: projects.ProjectID projectID) throws (1: ArgumentException missingFields)

    void    setNodeDefault(1: graphview.PortDescriptor dst, 
                           2: graph.DefaultValue       value, 
                           3: graph.NodeID             nodeID,
                           4: defs.DefID               defID, 
                           5: libs.LibID               libID,
                           6: projects.ProjectID       projectID) throws (1: ArgumentException missingFields)

    void removeNodeDefault(1: graphview.PortDescriptor dst, 
                           2: graph.NodeID             nodeID,
                           3: defs.DefID               defID, 
                           4: libs.LibID               libID,
                           5: projects.ProjectID       projectID) throws (1: ArgumentException missingFields)


    /*
     * File System
     */ 

    list<fs.FSItem> FS_ls   (1: string path)               throws (1: ArgumentException missingFields)
    fs.FSItem       FS_stat (1: string path)               throws (1: ArgumentException missingFields)
    void            FS_mkdir(1: string path)               throws (1: ArgumentException missingFields)
    void            FS_touch(1: string path)               throws (1: ArgumentException missingFields)
    void            FS_rm   (1: string path)               throws (1: ArgumentException missingFields)
    void            FS_cp   (1: string src, 2: string dst) throws (1: ArgumentException missingFields)
    void            FS_mv   (1: string src, 2: string dst) throws (1: ArgumentException missingFields)

    /*
     * Other
     */

    void ping()
    void dump()
    void shutdown()
}