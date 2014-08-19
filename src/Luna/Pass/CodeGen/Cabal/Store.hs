---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------

module Luna.Pass.CodeGen.Cabal.Store where

import           Control.Monad.RWS
import qualified System.IO         as IO

import qualified Flowbox.Luna.Data.Cabal.Config as CabalConfig
import           Flowbox.Prelude
import           Flowbox.System.IO.Serializer   (Serializable (Serializable))
import qualified Flowbox.System.IO.Serializer   as Serializer
import           Flowbox.System.UniPath         (UniPath)


type CabalConfig = CabalConfig.Config


run :: MonadIO m => CabalConfig -> UniPath -> m ()
run config path = liftIO $ Serializer.serialize item where
    cabal = CabalConfig.genCode config
    item  = Serializable path $ flip IO.hPutStr cabal
