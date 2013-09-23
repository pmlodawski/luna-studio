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
run config path = do
    liftIO $ store config path


store :: CabalConfig -> UniPath -> IO ()
store config path = do 
    let cabal = CabalConfig.genCode config
        s     = Serializable path (\h -> IO.hPutStr h cabal)
    Serializer.serialize s
