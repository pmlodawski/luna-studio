---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------

module Flowbox.Luna.Passes.CodeGen.Cabal.Store where

--import           Control.Monad.RWS                
--import qualified System.IO                      as IO

--import           Flowbox.Prelude                  
--import qualified Flowbox.Luna.Data.Cabal.Config as Config
--import           Flowbox.Luna.Data.Cabal.Config   (Config)
--import           Flowbox.System.UniPath           (UniPath)
--import qualified Flowbox.System.IO.Serializer   as Serializer
--import           Flowbox.System.IO.Serializer     (Serializable(Serializable))




--run :: MonadIO m => Config -> UniPath -> m ()
--run config path = do
--    liftIO $ storeCabal config path


--storeCabal :: Config -> UniPath -> IO ()
--storeCabal config path = do 
--    let cabal = Config.generate config
--        s     = Serializable path (\h -> IO.hPutStr h cabal)
--    Serializer.serialize s
