///////////////////////////////////////////////////////////////////////////
// Copyright (C) Flowbox, Inc / All Rights Reserved
// Unauthorized copying of this file, via any medium is strictly prohibited
// Proprietary and confidential
// Flowbox Team <contact@flowbox.io>, 2013
///////////////////////////////////////////////////////////////////////////

namespace cpp flowbox.batch.projects
namespace hs  flowbox.batch.projects

include "../../../../../../../../libs/luna/src/Flowbox/Luna/Tools/Serialize/Thrift/attrs.thrift"


typedef i32 ProjectID

struct Project {
    1: optional string           name
    2: optional string           path
    3: optional list<string>     libPaths  = []
	4: optional attrs.Attributes attribs   = {}
    5: optional ProjectID        projectID = -1
}
