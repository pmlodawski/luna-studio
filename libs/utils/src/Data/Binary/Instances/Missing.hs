---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Flowbox Team <contact@flowbox.io>, 2014
-- Proprietary and confidential
-- Unauthorized copying of this file, via any medium is strictly prohibited
---------------------------------------------------------------------------

module Data.Binary.Instances.Missing where

import           Control.Applicative ((<$>))
import           Data.Binary         (Binary (..))
import           Data.Text.Lazy      (Text)
import qualified Data.Text.Lazy      as Text
import           Prelude



instance Binary Text where
    put text = put $ Text.unpack text
    get = Text.pack <$> get

