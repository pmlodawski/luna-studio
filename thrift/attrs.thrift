///////////////////////////////////////////////////////////////////////////
// Copyright (C) Flowbox, Inc / All Rights Reserved
// Unauthorized copying of this file, via any medium is strictly prohibited
// Proprietary and confidential
// Flowbox Team <contact@flowbox.io>, 2013
///////////////////////////////////////////////////////////////////////////

namespace cpp flowbox.batch.attrs
namespace hs  flowbox.batch.attrs


struct Flags {
    1: optional bool io   = false
    2: optional bool omit = false
}

struct Attributes {
    1: optional map<string, map<string, string>> spaces = {}
}

