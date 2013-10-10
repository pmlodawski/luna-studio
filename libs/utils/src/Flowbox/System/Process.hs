-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------
{-# LANGUAGE Rank2Types #-}

module Flowbox.System.Process (
    module System.Process,

    readProcess,
    readProcess', -- original one from System.Process
    runProcess,
    runProcess', -- original one from System.Process
)where

import           Control.Applicative         
import qualified Control.Exception         as Exception
import qualified System.IO                 as IO
import qualified System.Exit               as Exit
import qualified System.Directory          as Directory
import qualified System.Process            as Process
import           System.Process            hiding (readProcess, runProcess)

import           Flowbox.Prelude           hiding (error)
import           Flowbox.System.Log.Logger   
import qualified Flowbox.System.UniPath    as UniPath
import           Flowbox.System.UniPath      (UniPath)
import           Data.String.Utils           (join)


logger :: LoggerIO
logger = getLoggerIO "Flowbox.System.Process"


runProcess' :: FilePath -> [String] -> Maybe FilePath -> Maybe [(String, String)] -> Maybe IO.Handle -> Maybe IO.Handle -> Maybe IO.Handle -> IO ProcessHandle
runProcess' = Process.runProcess


runProcess :: Maybe UniPath -> String -> [String] -> IO ()
runProcess upath command args = do
    let commandName = command ++ " " ++ (join " " args)
    workingDir <- case upath of 
        Nothing -> pure Nothing
        Just p  -> Just . UniPath.toUnixString <$> UniPath.expand p

    logger debug $ "Running command '" ++ commandName ++ "'"
    (_, out, err, pid) <- Process.runInteractiveProcess command args workingDir Nothing

    (_, e)   <- readOutput out err
    exitCode <- Process.waitForProcess pid
    if exitCode /= Exit.ExitSuccess 
        then fail $ "'" ++ commandName ++ "' returned with exit code: " ++ (show exitCode) ++ "\n" ++ e
        else pure ()


readProcess' :: FilePath -> [String] -> String -> IO String
readProcess' = Process.readProcess


readProcess :: Maybe UniPath -> String -> [String] -> String -> IO String
readProcess mpath command args input = do
    workingDir <- Directory.getCurrentDirectory
    case mpath of 
        Nothing    -> return ()
        Just upath -> do path <- UniPath.toUnixString <$> UniPath.expand upath
                         Directory.setCurrentDirectory path
    Exception.finally (Process.readProcess command args input)
                      (Directory.setCurrentDirectory workingDir)


readOutput :: IO.Handle -> IO.Handle -> IO (String, String)
readOutput hout herr = do
    let 
        readOutput1 :: IO.Handle -> IO.Handle -> String -> String -> IO (String, String)
        readOutput1 out err allOut allErr = do
            outEOF <- IO.hIsEOF out
            if not outEOF
                then do latestOut <- IO.hGetLine out
                        logger debug latestOut
                        readOutput1 out err (allOut ++ "\n" ++ latestOut) allErr
                else do errEOF <- IO.hIsEOF err
                        if not errEOF
                        then do latestErr <- IO.hGetLine err
                                logger debug latestErr
                                readOutput1 out err allOut (allErr ++ "\n" ++ latestErr)
                        else pure (allOut, allErr)

    IO.hSetBuffering hout IO.LineBuffering
    IO.hSetBuffering herr IO.LineBuffering

    readOutput1 hout herr "" ""