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

import           Flowbox.Control.Error                  (liftIO)
import           Flowbox.Prelude
import           Luna.Interpreter.Session.Env.State     (Session)
import qualified Luna.Interpreter.Session.Env.State     as Session
import           Luna.Interpreter.Session.Memory.Config (Config)
import qualified Luna.Interpreter.Session.Memory.Config as Config



data Status = BelowLimits        { _residency :: Int64 }
            | LowerLimitExceeded { _residency :: Int64 }
            | UpperLimitExceeded { _residency :: Int64 }
            deriving (Show)

makeLenses ''Status


fromResidency :: Config -> Int64 -> Status
fromResidency config res
    | res > config ^. Config.memoryUpperLimit = UpperLimitExceeded res
    | res > config ^. Config.memoryLowerLimit = LowerLimitExceeded res
    | otherwise                               = BelowLimits        res


isUpperLimitExceeded :: Status -> Bool
isUpperLimitExceeded (UpperLimitExceeded {}) = True
isUpperLimitExceeded _                       = False



currentBytesUsed :: IO Int64
currentBytesUsed = Stats.currentBytesUsed <$> Stats.getGCStats


status :: Session Status
status = fromResidency <$> Session.getMemoryConfig
                       <*> liftIO currentBytesUsed
