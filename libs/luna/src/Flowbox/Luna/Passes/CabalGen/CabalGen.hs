---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------

module Flowbox.Luna.Passes.CabalGen.CabalGen where

import           Flowbox.Prelude                            
import qualified Flowbox.Luna.Passes.CabalGen.CabalConfig as CabalConfig
import           Flowbox.Luna.Passes.CabalGen.CabalConfig   (CabalConfig)
import qualified Flowbox.Luna.Passes.CabalGen.CabalModule as CabalModule
import           Flowbox.Luna.Passes.CabalGen.CabalModule   (CabalModule)
import qualified Flowbox.System.UniPath   as UniPath
import           Flowbox.System.UniPath     (UniPath)



generateCabal :: CabalConfig -> UniPath -> IO ()
generateCabal config path = undefined