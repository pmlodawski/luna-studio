{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE FlexibleContexts #-}

module Main where

import           Prologue                         hiding (argument)
import           System.Environment               (getArgs)
import           System.Console.Docopt

import qualified Flowbox.Bus.EndPoint             as EP
import qualified Flowbox.Config.Config            as Config
import           Flowbox.System.Log.Logger
import qualified Empire.Monitor                   as Monitor

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

runOnIdle :: EP.BusEndPoints -> FilePath -> Int -> String -> IO ()
runOnIdle endPoints projectRoot time script = do
    rootLogger setIntLevel 3
    putStrLn $ "Script " <> script <> " scheduled to run after " <> show time <> " seconds of requests inactivity."
    r <- Monitor.run endPoints projectRoot time script
    case r of
        Left err -> logger criticalFail err
        _        -> return ()
