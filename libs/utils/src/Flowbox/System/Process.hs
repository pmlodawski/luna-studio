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
    let noStandardInput = ""
    (errorCode, stdOut, stdErr) <- Process.readProcessWithExitCode command args noStandardInput
    let runmsg = "Runing '" ++ command ++ " " ++ (List.concat $ List.intersperse " " args) ++ "':\n" ++ stdOut
    if errorCode == Exit.ExitSuccess
        then loggerIO info runmsg
        else do let errmsg = "Error running command '" ++ command ++ "'\n" ++ stdErr
                loggerIO warning runmsg
                loggerIO error   errmsg
                fail errmsg
