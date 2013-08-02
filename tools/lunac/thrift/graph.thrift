///////////////////////////////////////////////////////////////////////////
// Copyright (C) Flowbox, Inc / All Rights Reserved
// Unauthorized copying of this file, via any medium is strictly prohibited
// Proprietary and confidential
// Flowbox Team <contact@flowbox.io>, 2013
///////////////////////////////////////////////////////////////////////////


include "attrs.thrift"

typedef i32 NodeID

enum DefaultValueType {
    StringV,
    IntV
}

struct DefaultValue {
    1: required DefaultValueType cls,
    2: optional i32 i,
    3: optional string s
}

enum NodeType {
    Type,
    Call,
    Default,
    New,
    Inputs,
    Outputs,
    Tuple
}

struct Node {
    1: optional NodeType         cls
    2: optional string           name
    3: optional NodeID           nodeID
    4: optional attrs.Flags      flags
    5: optional attrs.Attributes attrs
    6: optional DefaultValue     defVal
}

typedef list<i32> PortDescriptor

struct Edge {
    1: optional NodeID src
    2: optional NodeID dst
}

struct Graph {
    1: optional map<NodeID, Node> nodes
    2: optional list<Edge>        edges
}

