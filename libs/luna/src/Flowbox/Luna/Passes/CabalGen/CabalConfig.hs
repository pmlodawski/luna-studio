---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------

module Flowbox.Luna.Passes.CabalGen.CabalConfig where

import           Flowbox.Prelude                            
import           Flowbox.Luna.Passes.CabalGen.CabalModule   (CabalModule)



data CabalConfig = CabalConfig { name         :: String
                               , version      :: String
                               , cabalVersion :: String
                               , author       :: String
                               , maintainer   :: String
                               , buildType    :: String
                               , cabalModules :: [CabalModule]
                               }