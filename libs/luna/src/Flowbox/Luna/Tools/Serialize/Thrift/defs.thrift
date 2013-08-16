///////////////////////////////////////////////////////////////////////////
// Copyright (C) Flowbox, Inc / All Rights Reserved
// Unauthorized copying of this file, via any medium is strictly prohibited
// Proprietary and confidential
// Flowbox Team <contact@flowbox.io>, 2013
///////////////////////////////////////////////////////////////////////////

namespace cpp flowbox.batch.defs
namespace hs  flowbox.batch.defs


include "attrs.thrift"
include "graph.thrift"
include "libs.thrift"
include "types.thrift"

typedef i32 DefID


struct Import {
    1: optional list<string> path
    2: optional list<string> items
}

typedef list<Import> Imports


struct Definition {
    1: optional types.Type       cls
    2: optional Imports          imports = []
    3: optional attrs.Flags      flags   = {}
    4: optional attrs.Attributes attribs = {}
    5: optional DefID            defID   = -1
}


struct DEdge {
    1: optional DefID src
    2: optional DefID dst
}


struct DefsGraph {
	1: optional map<DefID, Definition> defs
    2: optional list<DEdge>             edges
}


// structure for serialization to a single file - to remove in the future
struct DefManager {
    1: optional list<Definition>  defs
    2: optional list<graph.Graph> graphs
    3: optional list<DEdge>       edges
}