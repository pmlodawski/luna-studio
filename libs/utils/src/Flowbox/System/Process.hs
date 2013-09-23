-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------
{-# LANGUAGE Rank2Types #-}

module Flowbox.System.Process where

import qualified Data.List                 as List
import qualified System.Process            as Process
import qualified System.Exit               as Exit

import           Flowbox.Prelude           hiding (error)
import           Flowbox.System.Log.Logger   



runCommand :: String -> [String] -> LoggerIO -> IO ()
runCommand command args loggerIO = do
    let commandName = command ++ " " ++ (List.concat $ List.intersperse " " args)
        noStandardInput = ""
    loggerIO info $ "Runing '" ++ commandName ++ "'"
    (errorCode, stdOut, stdErr) <- Process.readProcessWithExitCode command args noStandardInput
    let runmsg = stdOut
    if errorCode == Exit.ExitSuccess
        then loggerIO info runmsg
        else do let errmsg = "Error running command '" ++ commandName ++ "'\n" ++ stdErr
                loggerIO warning runmsg
                loggerIO error   errmsg
                fail errmsg
