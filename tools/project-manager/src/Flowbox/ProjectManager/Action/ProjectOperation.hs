---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
module Flowbox.ProjectManager.Action.ProjectOperation where

import Flowbox.Prelude
import Flowbox.ProjectManager.Action.LibraryOperation (LibraryOperation)



data ProjectOperation = Create
                      | Open  
                      | Update
                      | Close 
                      | List  
                      | Lookup
                      | Store 
                      | Library LibraryOperation
                      deriving (Read, Show)

