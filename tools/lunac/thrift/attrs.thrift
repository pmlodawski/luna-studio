///////////////////////////////////////////////////////////////////////////
// Copyright (C) Flowbox, Inc / All Rights Reserved
// Unauthorized copying of this file, via any medium is strictly prohibited
// Proprietary and confidential
// Flowbox Team <contact@flowbox.io>, 2013
///////////////////////////////////////////////////////////////////////////


struct Flags {
    1: optional bool io
    2: optional bool omit
}

struct Attributes {
    1: optional map<string, map<string, string>>  spaces
}

