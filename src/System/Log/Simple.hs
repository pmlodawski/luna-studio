{-# LANGUAGE NoMonomorphismRestriction #-}

module System.Log.Simple where

import Prelude hiding (log)
import System.Log.Level
import System.Log.Data (log, empty)

simpleLog = log empty

debug     = simpleLog Debug
info      = simpleLog Info
notice    = simpleLog Notice
warning   = simpleLog Warning
error     = simpleLog Error
critical  = simpleLog Critical
alert     = simpleLog Alert
panic     = simpleLog Panic