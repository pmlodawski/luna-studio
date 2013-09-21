---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------

module Flowbox.Luna.Passes.CodeGen.Cabal.Run where

import           Control.Monad.RWS           
import qualified Control.Exception         as Exception
import qualified System.Directory          as Directory

import           Flowbox.Prelude           hiding (error)
import qualified Flowbox.System.Process    as Process
import qualified Flowbox.System.UniPath    as UniPath
import           Flowbox.System.UniPath      (UniPath)
import           Flowbox.System.Log.Logger   


loggerIO :: LoggerIO
loggerIO = getLoggerIO "Flowbox.Luna.Passes.Cabal.Run.RunCabal"


run :: MonadIO m => UniPath -> String -> [String] -> m ()
run projectPath name args = liftIO $ runCabal projectPath name args


runCabal :: UniPath -> String -> [String] -> IO ()
runCabal projectPath name args = do 
    workingDir <- Directory.getCurrentDirectory
    let runPath = UniPath.append ("build/hs/dist/build/" ++ name) projectPath
    Directory.setCurrentDirectory $ UniPath.toUnixString runPath

    Exception.finally (Process.runCommand ("./" ++ name) args loggerIO)
                      (Directory.setCurrentDirectory workingDir)
