---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------

module Flowbox.Luna.Passes.CabalGen.CabalGen where

import           Control.Monad.RWS                          
import qualified System.IO                                as IO

import           Flowbox.Prelude                            
import qualified Flowbox.Luna.Passes.CabalGen.CabalConfig as CabalConfig
import           Flowbox.Luna.Passes.CabalGen.CabalConfig   (CabalConfig)
import           Flowbox.System.UniPath                     (UniPath)
import qualified Flowbox.System.IO.Serializer             as Serializer
import           Flowbox.System.IO.Serializer               (Serializable(Serializable))




run :: MonadIO m => CabalConfig -> UniPath -> m ()
run config path = do
    liftIO $ generateCabal config path


generateCabal :: CabalConfig -> UniPath -> IO ()
generateCabal config path = do 
    let cabal = CabalConfig.generate config
        s     = Serializable path (\h -> IO.hPutStr h cabal)
    Serializer.serialize s
