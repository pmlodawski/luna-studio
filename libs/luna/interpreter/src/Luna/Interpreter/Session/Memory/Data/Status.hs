---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Flowbox Team <contact@flowbox.io>, 2014
-- Proprietary and confidential
-- Unauthorized copying of this file, via any medium is strictly prohibited
---------------------------------------------------------------------------
{-# LANGUAGE TemplateHaskell #-}

module Luna.Interpreter.Session.Memory.Data.Status where

import Data.Int (Int64)

import Flowbox.Prelude



data Status = BelowLimits        { _residency :: Int64 }
            | LowerLimitExceeded { _residency :: Int64 }
            | UpperLimitExceeded { _residency :: Int64 }
            deriving (Show)

makeLenses ''Status
