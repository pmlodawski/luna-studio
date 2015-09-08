---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE TemplateHaskell #-}

module Flowbox.Bus.Logger.Env where

import Data.Map.Strict (Map)
import Data.Time.Clock (UTCTime)

import qualified Flowbox.Bus.Data.Message as Message
import           Flowbox.Prelude



data Env = Env { _times :: !(Map Message.CorrelationID UTCTime) }
               deriving (Show)

makeLenses ''Env


instance Default Env where
    def = Env def

