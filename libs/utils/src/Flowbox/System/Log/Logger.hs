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

import qualified Flowbox.System.Log.LogEntry as LogEntry

type LogWriter m = MonadWriter [LogEntry.LogEntry] m

--getLogger :: LogWriter m => String -> (String -> m()) -> m()
getLogger name = \f -> f name

--getLoggerIO :: String -> (String -> IO()) -> IO()
getLoggerIO name = \f -> runLogger $ f name

runLogger :: Writer [LogEntry.LogEntry] a -> IO a
runLogger m = do
    let (out, entries) = runWriter m
    mapM_ logIO entries
    return out

log :: LogWriter m => Priority -> String -> String -> m()
log pri msg name = tell [LogEntry.LogEntry name pri msg]


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

--test :: (Enum a, MonadState a m, MonadWriter [LogEntry] m) => EitherT String m ()
--test = do
--    n <- get
--    logger.debug $ "o nie"
--    left "err"
--    put $ succ n
--    return ()
 

--main :: IO ()
--main = do
--    let y = runRWS (runEitherT test) 0 0
--    print $ y
--    return ()


--type Logger = (String -> IO ()) -> IO()


--getLogger :: String -> (String -> IO ()) -> IO()
--getLogger name = \f -> f name

--mkIndent :: Int -> String
--mkIndent i = replicate (4*i) ' '

--log :: Priority -> String -> String -> IO ()
--log pri msg name = do
--    conf <- Conf.read name
--    let sgr = case pri of
--                   DEBUG       -> [SetColor Foreground Vivid Magenta]
--                   INFO        -> [SetColor Foreground Vivid Green  ]
--                   NOTICE      -> [SetColor Foreground Vivid Cyan   ]
--                   WARNING     -> [SetColor Foreground Vivid Yellow ]
--                   ERROR       -> [SetColor Foreground Vivid Red    ]
--                   CRITICAL    -> [SetColor Foreground Vivid Red    ]
--                   ALERT       -> [SetColor Foreground Vivid Red    ]
--                   EMERGENCY   -> [SetColor Foreground Vivid Red    ]
--        prefix = mkIndent $ Conf.indent conf 
--    if Conf.colored conf then hSetSGR stderr sgr else return ()
--    logM name pri (prefix ++ msg)
--    if Conf.colored conf then hSetSGR stderr []  else return ()

--debug :: String -> String -> IO ()
--debug     = log DEBUG

--info :: String -> String -> IO ()
--info      = log INFO

--notice :: String -> String -> IO ()
--notice    = log NOTICE

--warning :: String -> String -> IO ()
--warning   = log WARNING

--error :: String -> String -> IO ()
--error     = log ERROR

--critical :: String -> String -> IO ()
--critical  = log CRITICAL

--alert :: String -> String -> IO ()
--alert     = log ALERT

--emergency :: String -> String -> IO ()
--emergency = log EMERGENCY

--setLevel :: Priority -> String -> IO ()
--setLevel lvl name = updateGlobalLogger name (HSLogger.setLevel lvl)

--pushLogGroup :: Logger -> IO()
--pushLogGroup l = l $ \name -> do
--    conf <- Conf.read name
--    Conf.store name conf{Conf.indent = 1 + Conf.indent conf}

--popLogGroup :: Logger -> IO()
--popLogGroup l = l $ \name -> do
--    conf <- Conf.read name
--    Conf.store name conf{Conf.indent = max 0 $ 1 - Conf.indent conf}

--enableColorOutput :: Bool -> Logger -> IO()
--enableColorOutput state l = l $ \name -> do
--    conf <- Conf.read name
--    Conf.store name conf{Conf.colored = state}