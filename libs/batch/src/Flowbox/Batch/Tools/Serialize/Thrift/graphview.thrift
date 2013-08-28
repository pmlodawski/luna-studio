///////////////////////////////////////////////////////////////////////////
// Copyright (C) Flowbox, Inc / All Rights Reserved
// Unauthorized copying of this file, via any medium is strictly prohibited
// Proprietary and confidential
// Flowbox Team <contact@flowbox.io>, 2013
///////////////////////////////////////////////////////////////////////////

namespace cpp flowbox.batch.graph
namespace hs  flowbox.batch.graph


include "../../../../../../../../libs/luna/src/Flowbox/Luna/Tools/Serialize/Thrift/graph.thrift"


typedef list<i32> PortDescriptor


struct EdgeView {
    1: optional graph.NodeID   srcNode
    2: optional graph.NodeID   dstNode
    3: optional PortDescriptor srcPort
    4: optional PortDescriptor dstPort
}

struct GraphView {
    1: optional map<graph.NodeID, graph.Node> nodes
    2: optional list<EdgeView>                edges
}

