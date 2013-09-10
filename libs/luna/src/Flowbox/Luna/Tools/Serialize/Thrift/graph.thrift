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
    2: optional string           value         
}

enum NodeType {
    Expr    = 0,
    Default = 1,
    Inputs  = 2,
    Outputs = 3,
    Tuple   = 4
}

struct Node {
    1: optional NodeType         cls
    2: optional string           expression = ""
    3: optional NodeID           nodeID     = -1
    4: optional attrs.Flags      flags      = {}
    5: optional attrs.Attributes attrs      = {}
    6: optional DefaultValue     defVal     = {}
}


enum PortType {
    All    = 0,
    Number = 1
}

struct Port {
    1: optional PortType cls
    2: optional i32      number
}

struct Edge {
    1: optional NodeID nodeSrc
    2: optional NodeID nodeDst
    3: optional Port   portSrc
    4: optional Port   portDst
}

struct Graph {
    1: optional map<NodeID, Node> nodes
    2: optional list<Edge>        edges
}

