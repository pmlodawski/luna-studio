---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------

module Flowbox.Luna.Passes.CabalGen.CabalModule where

import           Flowbox.Prelude   

data CabalModule = Library { hsSourceDirs   :: [String]
                           , ghcOptions     :: [String]
                           , extensions     :: [String]
                           , buildDepends   :: [String]
                           , exposedModules :: [String]
                           }
                 | Executable { name         :: String
                              , hsSourceDirs :: [String]
                              , ghcOptions   :: [String]
                              , extensions   :: [String]
                              , buildDepends :: [String]
                              , mainIs       :: String
                              }