///////////////////////////////////////////////////////////////////////////
// Copyright (C) Flowbox, Inc / All Rights Reserved
// Unauthorized copying of this file, via any medium is strictly prohibited
// Proprietary and confidential
// Flowbox Team <contact@flowbox.io>, 2013
///////////////////////////////////////////////////////////////////////////


include "attrs.thrift"

typedef i32 NodeID

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
    1: optional NodeType cls
    2: optional string name
    3: optional NodeID nodeID
    4: optional attrs.Flags flags
    5: optional attrs.Attributes attrs
}

struct Edge {
    1: i32 src
    2: i32 dst
}

typedef list<i32> PortDescriptor