---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------

{-# LANGUAGE FlexibleContexts, NoMonomorphismRestriction, ConstraintKinds, Rank2Types #-}

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
import           Data.DList                    (DList)

import qualified Flowbox.System.Log.LogEntry as LogEntry

import           Debug.Trace                   

type LogList     = DList LogEntry.LogEntry
type LogWriter m = MonadWriter LogList m

type LogAction b = LogWriter m => String -> String -> m b
type Logger      = forall t t1. (t1 -> String -> t) -> t1 -> t
type LoggerIO    = forall t. (t -> String -> Writer LogList ()) -> t -> IO ()

getLogger :: String -> Logger
getLogger name = \action msg -> action msg name

getLoggerIO :: String -> LoggerIO
getLoggerIO name = \action msg -> runLogger $ action msg name

runLogger :: Writer LogList a -> IO a
runLogger m = do
    let (out, entries) = runWriter m
    mapM_ logIO $ DList.toList entries
    return out

log :: LogWriter m => Priority -> String -> String -> m()
log pri msg name = tell $ DList.singleton (LogEntry.LogEntry name pri msg)

append :: MonadWriter w m => w -> m ()
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

debug :: LogAction ()
debug = log DEBUG

info :: LogAction ()
info = log INFO

notice :: LogAction ()
notice = log NOTICE

warning :: LogAction ()
warning = log WARNING

error :: LogAction ()
error = log ERROR

critical :: LogAction ()
critical = log CRITICAL

alert :: LogAction ()
alert = log ALERT

emergency :: LogAction ()
emergency = log EMERGENCY 

criticalFail :: LogAction b
criticalFail msg name = do
    log CRITICAL msg name
    fail msg

setLevel :: Priority -> String -> IO ()
setLevel lvl name = updateGlobalLogger name (HSLogger.setLevel lvl)
