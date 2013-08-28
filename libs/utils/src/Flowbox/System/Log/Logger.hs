---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------

module Flowbox.System.Log.Logger (
    module Flowbox.System.Log.Logger,
    Priority(..)
)where

import qualified System.Log.Logger       as HSLogger
import           System.Log.Logger       hiding (getLogger, setLevel, Logger)
import           System.IO                 (stderr)
import           Prelude                 hiding (log, error)
import           System.Console.ANSI     as ANSI
import qualified Flowbox.System.Log.Conf as Conf

type Logger = (String -> IO ()) -> IO()


getLogger :: String -> (String -> IO ()) -> IO()
getLogger name = \f -> f name

mkIndent :: Int -> String
mkIndent i = replicate (4*i) ' '

log :: Priority -> String -> String -> IO ()
log pri msg name = do
    conf <- Conf.read name
    let sgr = case pri of
                   DEBUG       -> [SetColor Foreground Vivid Magenta]
                   INFO        -> [SetColor Foreground Vivid Green  ]
                   NOTICE      -> [SetColor Foreground Vivid Cyan   ]
                   WARNING     -> [SetColor Foreground Vivid Yellow ]
                   ERROR       -> [SetColor Foreground Vivid Red    ]
                   CRITICAL    -> [SetColor Foreground Vivid Red    ]
                   ALERT       -> [SetColor Foreground Vivid Red    ]
                   EMERGENCY   -> [SetColor Foreground Vivid Red    ]
        prefix = mkIndent $ Conf.indent conf 
    if Conf.colored conf then hSetSGR stderr sgr else return ()
    logM name pri (prefix ++ msg)
    if Conf.colored conf then hSetSGR stderr []  else return ()

debug :: String -> String -> IO ()
debug     = log DEBUG

info :: String -> String -> IO ()
info      = log INFO

notice :: String -> String -> IO ()
notice    = log NOTICE

warning :: String -> String -> IO ()
warning   = log WARNING

error :: String -> String -> IO ()
error     = log ERROR

critical :: String -> String -> IO ()
critical  = log CRITICAL

alert :: String -> String -> IO ()
alert     = log ALERT

emergency :: String -> String -> IO ()
emergency = log EMERGENCY

setLevel :: Priority -> String -> IO ()
setLevel lvl name = updateGlobalLogger name (HSLogger.setLevel lvl)

pushLogGroup :: Logger -> IO()
pushLogGroup l = l $ \name -> do
    conf <- Conf.read name
    Conf.store name conf{Conf.indent = 1 + Conf.indent conf}

popLogGroup :: Logger -> IO()
popLogGroup l = l $ \name -> do
    conf <- Conf.read name
    Conf.store name conf{Conf.indent = max 0 $ 1 - Conf.indent conf}

enableColorOutput :: Bool -> Logger -> IO()
enableColorOutput state l = l $ \name -> do
    conf <- Conf.read name
    Conf.store name conf{Conf.colored = state}