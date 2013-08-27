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

import qualified System.Log.Logger   as HSLogger
import           System.Log.Logger   hiding (getLogger, setLevel)
import           System.IO             (stderr)
import           Prelude             hiding (log, error)
import           System.Console.ANSI as ANSI


getLogger :: String -> (String -> IO ()) -> IO()
getLogger name = \f -> f name

log :: Priority -> String -> String -> IO ()
log pri msg name = do
    let sgr = case pri of
                   DEBUG       -> [SetColor Foreground Vivid Magenta]
                   INFO        -> [SetColor Foreground Vivid Green  ]
                   NOTICE      -> [SetColor Foreground Vivid Cyan   ]
                   WARNING     -> [SetColor Foreground Vivid Yellow ]
                   ERROR       -> [SetColor Foreground Vivid Red    ]
                   CRITICAL    -> [SetColor Foreground Vivid Red    ]
                   ALERT       -> [SetColor Foreground Vivid Red    ]
                   EMERGENCY   -> [SetColor Foreground Vivid Red    ]
    hSetSGR stderr sgr
    logM name pri msg
    hSetSGR stderr []

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