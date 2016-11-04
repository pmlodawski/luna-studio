{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

module Main where

import           Prologue                  hiding (argument)
import           System.Console.Docopt
import           System.Environment        (getArgs)

import qualified Empire.Monitor            as Monitor
import qualified ZMQ.Bus.Config     as Config
import           Flowbox.System.Log.Logger
import qualified ZMQ.Bus.EndPoint          as EP

patterns :: Docopt
patterns = [docoptFile|src/RequestMonitorUsage.txt|]

getArgOrExit = getArgOrExitWith patterns

rootLogger :: Logger
rootLogger = getLogger ""

logger :: LoggerIO
logger = getLoggerIO $moduleName

main :: IO ()
main = do
    args <- parseArgsOrExit patterns =<< getArgs
    endPoints <- EP.clientFromConfig <$> Config.load
    projectRoot <- Config.projectRoot <$> Config.projects <$> Config.load
    print projectRoot
    when (args `isPresent` command "runOnIdle") $ do
        time       <- args `getArgOrExit` argument "seconds"
        script     <- args `getArgOrExit` argument "script"
        runOnIdle endPoints projectRoot (read time) script

runOnIdle :: EP.BusEndPoints -> FilePath -> Integer -> FilePath -> IO ()
runOnIdle endPoints projectRoot time script = do
    rootLogger setIntLevel 3
    r <- Monitor.run endPoints projectRoot time script
    case r of
        Left err -> logger criticalFail err
        _        -> return ()
