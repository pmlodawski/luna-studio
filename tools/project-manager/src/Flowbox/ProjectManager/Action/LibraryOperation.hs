---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
module Flowbox.ProjectManager.Action.LibraryOperation where

import Flowbox.Prelude


data LibraryOperation = Create
                      | Load  
                      | Update-- ?
                      | Unload
                      | List  
                      | Lookup
                      | Store 
                      deriving (Read, Show)
