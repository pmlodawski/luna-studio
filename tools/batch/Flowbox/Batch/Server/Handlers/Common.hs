---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------
module Flowbox.Batch.Server.Handlers.Common where

import           Control.Exception
import           Data.Text.Lazy     (pack)

import           Batch_Types (ArgumentException(..))


throw' :: String -> c
throw' = throw . ArgumentException . Just . pack