---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------

{-# LANGUAGE FlexibleContexts, NoMonomorphismRestriction, ConstraintKinds #-}

module Flowbox.System.Log.Logger (
    module Flowbox.System.Log.Logger,
    Priority(..)
)where

import qualified System.Log.Logger           as HSLogger
import           System.IO                     (stderr)
import           System.Console.ANSI         as ANSI


import           Control.Monad.State           
import           Control.Monad.Writer          
import           System.Log.Logger           hiding (getLogger, setLevel, Logger)
import           Prelude                     hiding (log, fail)
import qualified Data.DList                  as DList
import           Data.DList                  (DList)

import qualified Flowbox.System.Log.LogEntry as LogEntry

import Debug.Trace

type LogList     = DList LogEntry.LogEntry
type LogWriter m = MonadWriter LogList m

--getLogger :: LogWriter m => String -> (String -> m()) -> m()
getLogger name = \f -> f name

--getLoggerIO :: String -> (String -> IO()) -> IO()
getLoggerIO name = \f -> runLogger $ f name

runLogger :: Writer LogList a -> IO a
runLogger m = do
    let (out, entries) = runWriter m
    mapM_ logIO $ DList.toList entries
    return out

log :: LogWriter m => Priority -> String -> String -> m()
log pri msg name = tell $ DList.singleton (LogEntry.LogEntry name pri msg)

append = tell

logIO :: LogEntry.LogEntry -> IO ()
logIO entry = do
    --conf <- Conf.read name
    let name  = LogEntry.name     entry
        msg   = LogEntry.msg      entry
        pri   = LogEntry.priority entry
        sgr   = case pri of
                   DEBUG       -> [SetColor Foreground Vivid Magenta]
                   INFO        -> [SetColor Foreground Vivid Green  ]
                   NOTICE      -> [SetColor Foreground Vivid Cyan   ]
                   WARNING     -> [SetColor Foreground Vivid Yellow ]
                   ERROR       -> [SetColor Foreground Vivid Red    ]
                   CRITICAL    -> [SetColor Foreground Vivid Red    ]
                   ALERT       -> [SetColor Foreground Vivid Red    ]
                   EMERGENCY   -> [SetColor Foreground Vivid Red    ]
        --prefix = mkIndent $ Conf.indent conf 
    hSetSGR stderr sgr
    --if Conf.colored conf then hSetSGR stderr sgr else return ()
    logM name pri (msg)
    hSetSGR stderr []
    --if Conf.colored conf then hSetSGR stderr []  else return ()

debug :: LogWriter m => String -> String -> m()
debug = log DEBUG

info :: LogWriter m => String -> String -> m()
info = log INFO

notice :: LogWriter m => String -> String -> m()
notice = log NOTICE

warning :: LogWriter m => String -> String -> m()
warning = log WARNING

error :: LogWriter m => String -> String -> m()
error = log ERROR

critical :: LogWriter m => String -> String -> m()
critical = log CRITICAL

alert :: LogWriter m => String -> String -> m()
alert = log ALERT

emergency :: LogWriter m => String -> String -> m()
emergency = log EMERGENCY 

criticalFail :: LogWriter m => String -> String -> m b
criticalFail msg name = do
    log CRITICAL msg name
    fail msg

setLevel :: Priority -> String -> IO ()
setLevel lvl name = updateGlobalLogger name (HSLogger.setLevel lvl)
