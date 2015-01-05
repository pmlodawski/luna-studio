///////////////////////////////////////////////////////////////////////////
// Copyright (C) Flowbox, Inc / All Rights Reserved
// Unauthorized copying of this file, via any medium is strictly prohibited
// Proprietary and confidential
// Flowbox Team <contact@flowbox.io>, 2014
///////////////////////////////////////////////////////////////////////////

// StartEnd.c
#include <Rts.h>

void HsStart()
{
   int argc = 4;
   char* argv[] = {"ghcDll", "-v5", "+RTS", "-T", NULL}; // argv must end with NULL

   // Initialize Haskell runtime
   char** args = argv;
   hs_init(&argc, &args);
}

void HsEnd()
{
   hs_exit();
}