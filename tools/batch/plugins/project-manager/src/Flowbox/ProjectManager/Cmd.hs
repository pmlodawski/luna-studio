---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------

module Flowbox.ProjectManager.Cmd where

import Flowbox.Prelude



data Cmd = Run { verbose :: Int
               , noColor :: Bool
               }
         | Version
         deriving Show
