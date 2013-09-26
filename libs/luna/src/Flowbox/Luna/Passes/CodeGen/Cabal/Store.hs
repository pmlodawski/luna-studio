---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------

module Flowbox.Luna.Passes.CodeGen.Cabal.Store where

import           Control.Monad.RWS                
import qualified System.IO                      as IO

import           Flowbox.Prelude                  
import qualified Flowbox.Luna.Data.Cabal.Config as CabalConfig
import           Flowbox.System.UniPath           (UniPath)
import qualified Flowbox.System.IO.Serializer   as Serializer
import           Flowbox.System.IO.Serializer     (Serializable(Serializable))


type CabalConfig = CabalConfig.Config


run :: MonadIO m => CabalConfig -> UniPath -> m ()
run config path = liftIO $ Serializer.serialize item where
    cabal = CabalConfig.genCode config
    item  = Serializable path $ flip IO.hPutStr cabal
