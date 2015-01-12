module System.Log.Level where

import Prelude hiding (log, lookup)

data Level = DEBUG     -- ^ Debug Logs
           | INFO      -- ^ Information
           | NOTICE    -- ^ Normal runtime conditions
           | WARNING   -- ^ General Warnings
           | ERROR     -- ^ General Errors
           | CRITICAL  -- ^ Severe situations
           | ALERT     -- ^ Take immediate action
           | PANIC     -- ^ System is unusable
           deriving (Eq, Ord, Show, Read, Enum)


