///////////////////////////////////////////////////////////////////////////
// Copyright (C) Flowbox, Inc / All Rights Reserved
// Unauthorized copying of this file, via any medium is strictly prohibited
// Proprietary and confidential
// Flowbox Team <contact@flowbox.io>, 2013
///////////////////////////////////////////////////////////////////////////

namespace cpp flowbox.batch.graph
namespace hs  flowbox.batch.graph


include "attrs.thrift"

typedef i32 NodeID

enum DefaultValueType {
    CharV,
    IntV,
    StringV
}

struct DefaultValue {
    1: required DefaultValueType cls,
    3: optional string s
}

enum NodeType {
    Expr,
    Default,
    Inputs,
    Outputs,
    NTuple
}

struct Node {
    1: optional NodeType         cls
    2: optional string           expression = ""
    3: optional NodeID           nodeID     = -1
    4: optional attrs.Flags      flags      = {}
    5: optional attrs.Attributes attrs      = {}
    6: optional DefaultValue     defVal     = {}
}

struct Edge {
    2: optional i32 portDst
    3: optional NodeID nodeSrc
    4: optional NodeID nodeDst
}

struct Graph {
    1: optional map<NodeID, Node> nodes
    2: optional list<Edge>        edges
}

