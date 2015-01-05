///////////////////////////////////////////////////////////////////////////
// Copyright (C) Flowbox, Inc / All Rights Reserved
// Unauthorized copying of this file, via any medium is strictly prohibited
// Proprietary and confidential
// Flowbox Team <contact@flowbox.io>, 2014
///////////////////////////////////////////////////////////////////////////

// Tester.cpp
#include "HsFFI.h"
#include "../interpreter/dist/dist-sandbox-2a78fc06/build/Main_stub.h"
#include <stdio.h>


extern "C" {
    void HsStart();
    void HsEnd();
}

int main()
{
    HsStart();
    // can now safely call functions from the DLL
    runInterpreter();
    HsEnd();
    return 0;
}