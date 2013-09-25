---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------

module Flowbox.Luna.Passes.CodeGen.Cabal.Build where

import           Control.Monad.RWS           

import           Flowbox.Prelude           hiding (error)
import qualified Flowbox.System.Process    as Process
import           Flowbox.System.UniPath      (UniPath)
import           Flowbox.System.Log.Logger   


loggerIO :: LoggerIO
loggerIO = getLoggerIO "Flowbox.Luna.Passes.CodeGen.Cabal.Build"


run :: MonadIO m => UniPath -> m ()
run buildPath = liftIO $ Process.runCommandInFolder buildPath "cabal" ["build"] 
    

