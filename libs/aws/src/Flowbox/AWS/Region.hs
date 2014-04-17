---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------

module Flowbox.AWS.Region where

import           Data.Text (Text)
import qualified Data.Text as Text

import Flowbox.Prelude



type Region = Text


mk :: String -> Text
mk = Text.pack
