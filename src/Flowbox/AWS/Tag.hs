---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
module Flowbox.AWS.Tag (
    Tag,
    Key,
    Value,

    module X,
) where

import           Data.Text as X hiding (Text, filter)
import qualified Data.Text as Text



type Key = Text.Text


type Value = Text.Text


type Tag = (Key, Value)
