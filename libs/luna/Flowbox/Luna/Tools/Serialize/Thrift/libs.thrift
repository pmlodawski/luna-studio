///////////////////////////////////////////////////////////////////////////
// Copyright (C) Flowbox, Inc / All Rights Reserved
// Unauthorized copying of this file, via any medium is strictly prohibited
// Proprietary and confidential
// Flowbox Team <contact@flowbox.io>, 2013
///////////////////////////////////////////////////////////////////////////

namespace cpp flowbox.batch.libs
namespace hs  flowbox.batch.libs


typedef i32 LibID


struct Library {
    1: optional LibID  libID = -1
    2: optional string name
    3: optional string path
    4: optional i32    rootDefID = -1
}