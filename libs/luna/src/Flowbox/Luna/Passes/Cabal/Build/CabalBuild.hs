---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------

module Flowbox.Luna.Passes.Cabal.Build.CabalBuild where

import           Control.Monad.RWS           
import qualified Control.Exception         as Exception
import qualified Data.List                 as List
import qualified System.Directory          as Directory
import qualified System.Process            as Process
import qualified System.Exit               as Exit

import           Flowbox.Prelude           hiding (error)
import qualified Flowbox.System.UniPath    as UniPath
import           Flowbox.System.UniPath      (UniPath)
import           Flowbox.System.Log.Logger   


loggerIO :: LoggerIO
loggerIO = getLoggerIO "Flowbox.Luna.Passes.Cabal.BuildCabal"


run :: MonadIO m => UniPath -> m ()
run = liftIO . buildCabal


runCommand :: String -> [String] -> IO ()
runCommand command args = do
    let noStandardInput = ""
    (errorCode, stdOut, stdErr) <- Process.readProcessWithExitCode command args noStandardInput
    let runmsg = "Runing '" ++ command ++ " " ++ (List.concat $ List.intersperse " " args) ++ "':\n" ++ stdOut
    if errorCode == Exit.ExitSuccess
        then loggerIO info runmsg
        else do let errmsg = "Error running command '" ++ command ++ "'\n" ++ stdErr
                loggerIO warning runmsg
                loggerIO error   errmsg
                fail errmsg


buildCabal :: UniPath -> IO ()
buildCabal projectPath = do 
    workingDir <- Directory.getCurrentDirectory
    let buildPath = UniPath.append "build/hs" projectPath
    Directory.setCurrentDirectory $ UniPath.toUnixString buildPath

    Exception.finally (do runCommand "cabal" ["configure"]
                          runCommand "cabal" ["build"])
                      (Directory.setCurrentDirectory workingDir)
