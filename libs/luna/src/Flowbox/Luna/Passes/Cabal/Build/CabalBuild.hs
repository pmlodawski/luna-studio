---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------

module Flowbox.Luna.Passes.Cabal.Build.CabalBuild where

import           Control.Monad.RWS                       
import qualified System.IO                             as IO

import           Flowbox.Prelude                         
import qualified Flowbox.Luna.Passes.Cabal.CabalConfig as CabalConfig
import           Flowbox.Luna.Passes.Cabal.CabalConfig   (CabalConfig)
import           Flowbox.System.UniPath                  (UniPath)
import qualified Flowbox.System.IO.Serializer          as Serializer
import           Flowbox.System.IO.Serializer            (Serializable(Serializable))




run :: MonadIO m => UniPath -> m ()
run = liftIO . buildCabal


buildCabal :: UniPath -> IO ()
buildCabal path = do 
    return undefined