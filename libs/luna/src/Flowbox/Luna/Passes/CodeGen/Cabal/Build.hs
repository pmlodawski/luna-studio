---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------

module Flowbox.Luna.Passes.CodeGen.Cabal.Build where

import           Control.Monad.RWS           
import qualified Control.Exception         as Exception
import qualified System.Directory          as Directory

import           Flowbox.Prelude           hiding (error)
import qualified Flowbox.System.Process    as Process
import qualified Flowbox.System.UniPath    as UniPath
import           Flowbox.System.UniPath      (UniPath)
import           Flowbox.System.Log.Logger   


loggerIO :: LoggerIO
loggerIO = getLoggerIO "Flowbox.Luna.Passes.CodeGen.Cabal.Build"


run :: MonadIO m => UniPath -> m ()
run = liftIO . build


build :: UniPath -> IO ()
build buildPath = do 
    workingDir <- Directory.getCurrentDirectory
    Directory.setCurrentDirectory $ UniPath.toUnixString buildPath

    Exception.finally (do Process.runCommand "cabal" ["configure"] loggerIO
                          Process.runCommand "cabal" ["build"] loggerIO)
                      (Directory.setCurrentDirectory workingDir)
