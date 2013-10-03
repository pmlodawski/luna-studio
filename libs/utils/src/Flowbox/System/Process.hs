-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------
{-# LANGUAGE Rank2Types #-}

module Flowbox.System.Process (
    module System.Process,

    readProcessInFolder,
    runProcessInFolder,
)where

import           Control.Applicative         
import qualified Control.Exception         as Exception
import qualified Data.Maybe                as Maybe
import qualified System.IO                 as IO
import qualified System.Exit               as Exit
import qualified System.Directory          as Directory
import qualified System.Process            as Process
import           System.Process              

import           Flowbox.Prelude           hiding (error)
import           Flowbox.System.Log.Logger   
import qualified Flowbox.System.UniPath    as UniPath
import           Flowbox.System.UniPath      (UniPath)
import           Data.String.Utils           (join)


loggerIO :: LoggerIO
loggerIO = getLoggerIO "Flowbox.System.Process"


readOutput :: IO.Handle -> IO.Handle -> IO (String, String)
readOutput hout herr = do
    let 
        readOutput1 :: IO.Handle -> IO.Handle -> String -> String -> IO (String, String)
        readOutput1 out err allOut allErr = do
            outEOF <- IO.hIsEOF out
            if not outEOF
                then do latestOut <- IO.hGetLine out
                        loggerIO debug latestOut
                        readOutput1 out err (allOut ++ "\n" ++ latestOut) allErr
                else do errEOF <- IO.hIsEOF err
                        if not errEOF
                        then do latestErr <- IO.hGetLine err
                                loggerIO error latestErr
                                readOutput1 out err allOut (allErr ++ "\n" ++ latestErr)
                        else return (allOut, allErr)

    IO.hSetBuffering hout IO.LineBuffering
    IO.hSetBuffering herr IO.LineBuffering

    readOutput1 hout herr "" ""


runProcessInFolder :: UniPath -> String -> [String] -> IO ()
runProcessInFolder upath command args  = do
    let commandName = command ++ " " ++ (join " " args)
    workingDir     <- UniPath.toUnixString <$> UniPath.expand upath
    loggerIO debug $ "Running command '" ++ commandName ++ "'"
    (_, out, err, pid) <- Process.runInteractiveProcess command args (Just workingDir) Nothing
    
    (_, e)   <- readOutput out err
    exitCode <- Process.getProcessExitCode pid
    if exitCode /= Just Exit.ExitSuccess
        then fail $ "'" ++ commandName ++ "' returned with exit code: " ++ (show $ Maybe.fromJust exitCode) ++ "\n" ++ e
        else return ()


readProcessInFolder :: UniPath -> String -> [String] -> String -> IO String
readProcessInFolder upath command args input = do
    workingDir <- Directory.getCurrentDirectory
    path       <- UniPath.toUnixString <$> UniPath.expand upath
    Directory.setCurrentDirectory path

    Exception.finally (Process.readProcess command args input)
                      (Directory.setCurrentDirectory workingDir)