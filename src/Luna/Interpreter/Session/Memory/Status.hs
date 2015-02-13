---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Flowbox Team <contact@flowbox.io>, 2014
-- Proprietary and confidential
-- Unauthorized copying of this file, via any medium is strictly prohibited
---------------------------------------------------------------------------
{-# LANGUAGE TemplateHaskell #-}

module Luna.Interpreter.Session.Memory.Status where

import           Data.Int  (Int64)
import qualified GHC.Stats as Stats

import           Flowbox.Control.Error                       (safeLiftIO')
import           Flowbox.Prelude
import           Flowbox.Source.Location                     (loc)
import           Luna.Interpreter.Session.Env.Session        (Session)
import qualified Luna.Interpreter.Session.Env.State          as Session
import qualified Luna.Interpreter.Session.Error              as Error
import           Luna.Interpreter.Session.Memory.Config      (Config)
import qualified Luna.Interpreter.Session.Memory.Config      as Config
import           Luna.Interpreter.Session.Memory.Data.Status (Status)
import qualified Luna.Interpreter.Session.Memory.Data.Status as Status



fromResidency :: Config -> Int64 -> Status
fromResidency config res
    | res > config ^. Config.memoryUpperLimit = Status.UpperLimitExceeded res
    | res > config ^. Config.memoryLowerLimit = Status.LowerLimitExceeded res
    | otherwise                               = Status.BelowLimits        res


isUpperLimitExceeded :: Status -> Bool
isUpperLimitExceeded (Status.UpperLimitExceeded {}) = True
isUpperLimitExceeded _                              = False


isLowerLimitExceeded :: Status -> Bool
isLowerLimitExceeded (Status.UpperLimitExceeded {}) = True
isLowerLimitExceeded (Status.LowerLimitExceeded {}) = True
isLowerLimitExceeded _                              = False


isUpperLimitExceeded' :: Session mm Bool
isUpperLimitExceeded' = isUpperLimitExceeded <$> status


isLowerLimitExceeded' :: Session mm Bool
isLowerLimitExceeded' = isLowerLimitExceeded <$> status


currentBytesUsed :: IO Int64
currentBytesUsed = Stats.currentBytesUsed <$> Stats.getGCStats


status :: Session mm Status
status = fromResidency <$> Session.getMemoryConfig
                       <*> safeLiftIO' (Error.IOError $(loc)) currentBytesUsed
