///////////////////////////////////////////////////////////////////////////
// Copyright (C) Flowbox, Inc / All Rights Reserved
// Unauthorized copying of this file, via any medium is strictly prohibited
// Proprietary and confidential
// Flowbox Team <contact@flowbox.io>, 2013
///////////////////////////////////////////////////////////////////////////

include "attrs.thrift"
include "libs.thrift"
include "types.thrift"

struct NodeDefinition {
    1: optional types.TypeContainer cls
    2: optional libs.LibID libID
    3: optional attrs.Flags flags
    4: optional attrs.Attributes attrs
}
