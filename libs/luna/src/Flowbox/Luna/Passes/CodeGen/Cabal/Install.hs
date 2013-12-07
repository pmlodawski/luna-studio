---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------

module Flowbox.Luna.Passes.CodeGen.Cabal.Install where

import Control.Monad.RWS

import           Flowbox.Config.Config     (Config)
import qualified Flowbox.Config.Config     as Config
import           Flowbox.Prelude           hiding (error)
import           Flowbox.System.Log.Logger
import qualified Flowbox.System.Process    as Process
import           Flowbox.System.UniPath    (UniPath)

import Control.Applicative



loggerIO :: LoggerIO
loggerIO = getLoggerIO "Flowbox.Luna.Passes.CodeGen.Cabal.Install"


run :: MonadIO m => Config -> UniPath -> [String] -> m ()
run config location flags = liftIO $ Process.runProcess (Just location) command args where
    command = Config.cabal $ Config.wrappers config
    args    = "install" : flags



