---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------

module Flowbox.System.Log.Priority where

import qualified System.Log.Logger           as HSLogger
import           Flowbox.Prelude  


data Priority = TRACE
              | DEBUG
              | INFO
              | WARNING
              | ERROR
              | CRITICAL
              deriving (Show, Ord, Eq)

pri2hspri p = case p of
    CRITICAL -> HSLogger.CRITICAL
    ERROR    -> HSLogger.ERROR
    WARNING  -> HSLogger.WARNING
    INFO     -> HSLogger.NOTICE
    DEBUG    -> HSLogger.INFO
    TRACE    -> HSLogger.DEBUG


hspri2pri p = case p of
    HSLogger.CRITICAL -> CRITICAL     
    HSLogger.ERROR    -> ERROR     
    HSLogger.WARNING  -> WARNING     
    HSLogger.NOTICE   -> INFO       
    HSLogger.INFO     -> DEBUG    
    HSLogger.DEBUG    -> TRACE   