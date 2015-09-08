---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Flowbox Team <contact@flowbox.io>, 2014
-- Proprietary and confidential
-- Unauthorized copying of this file, via any medium is strictly prohibited
---------------------------------------------------------------------------
{-# LANGUAGE TemplateHaskell #-}

module Luna.Interpreter.Session.Memory.Config where

import Data.Int (Int64)

import Flowbox.Prelude



data Config = Config { _memoryUpperLimit :: Int64
                     , _memoryLowerLimit :: Int64
                     } deriving (Show)

makeLenses ''Config


instance Default Config where
    def = Config defaultMemoryUpperLimit defaultMemoryLowerLimit


defaultMemoryUpperLimit :: Int64
defaultMemoryUpperLimit = 1000 * 10^6


defaultMemoryLowerLimit :: Int64
defaultMemoryLowerLimit =  800 * 10^6
