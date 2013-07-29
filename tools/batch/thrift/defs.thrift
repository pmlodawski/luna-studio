///////////////////////////////////////////////////////////////////////////
// Copyright (C) Flowbox, Inc / All Rights Reserved
// Unauthorized copying of this file, via any medium is strictly prohibited
// Proprietary and confidential
// Flowbox Team <contact@flowbox.io>, 2013
///////////////////////////////////////////////////////////////////////////


include "attrs.thrift"
include "libs.thrift"
include "types.thrift"

typedef i32 DefID

struct NodeDefinition {
    1: optional types.Type       cls
    2: optional attrs.Flags      flags
    3: optional attrs.Attributes attribs
    4: optional libs.LibID       libID   = -1
    5: optional DefID            defID   = -1
}
