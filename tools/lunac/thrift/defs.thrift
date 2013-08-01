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


struct Import {
    1: optional list<string> path
    2: optional list<string> items
}

typedef list<Import> Imports


struct NodeDef {
    1: optional types.Type       cls
    2: optional Imports          imports
    3: optional attrs.Flags      flags
    4: optional attrs.Attributes attribs
    5: optional libs.LibID       libID   = -1
    6: optional DefID            defID   = -1
}
