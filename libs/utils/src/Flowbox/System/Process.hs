-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------
{-# LANGUAGE Rank2Types #-}

module Flowbox.System.Process where

import           Control.Applicative         
import qualified Control.Exception         as Exception
import qualified Data.List                 as List
import qualified System.Directory          as Directory
import qualified System.Process            as Process
import qualified System.Exit               as Exit

import           Flowbox.Prelude           hiding (error)
import           Flowbox.System.Log.Logger   
import qualified Flowbox.System.UniPath    as UniPath
import           Flowbox.System.UniPath      (UniPath)


loggerIO :: LoggerIO
loggerIO = getLoggerIO "Flowbox.System.Process"


runCommand :: String -> [String] -> IO ()
runCommand command args  = do
    let commandName = command ++ " " ++ (List.concat $ List.intersperse " " args)
        noStandardInput = ""
    loggerIO debug $ "Running command '" ++ commandName ++ "'"
    (errorCode, stdOut, stdErr) <- Process.readProcessWithExitCode command args noStandardInput
    let runmsg   = stdOut
    if errorCode == Exit.ExitSuccess
        then loggerIO info runmsg
        else do let errmsg = "Error running command '" ++ commandName ++ "'\n" ++ stdErr
                loggerIO error errmsg
                fail errmsg


runCommandInFolder :: UniPath -> String -> [String] -> IO ()
runCommandInFolder upath command args  = do
    workingDir <- Directory.getCurrentDirectory
    path       <- UniPath.toUnixString <$> UniPath.expand upath
    Directory.setCurrentDirectory path

    Exception.finally (runCommand command args)
                      (Directory.setCurrentDirectory workingDir)
