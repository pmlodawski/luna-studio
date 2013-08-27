///////////////////////////////////////////////////////////////////////////
// Copyright (C) Flowbox, Inc / All Rights Reserved
// Unauthorized copying of this file, via any medium is strictly prohibited
// Proprietary and confidential
// Flowbox Team <contact@flowbox.io>, 2013
///////////////////////////////////////////////////////////////////////////

namespace cpp flowbox.batch.fs
namespace hs  flowbox.batch.fs


enum FSItemType {
    Directory,
    File,
    Other
}


struct FSItem {
    1: optional FSItemType itemType
    2: optional string     path
    3: optional i32        size
}

