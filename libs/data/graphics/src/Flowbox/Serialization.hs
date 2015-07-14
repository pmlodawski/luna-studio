---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE MultiParamTypeClasses #-}

module Flowbox.Serialization (
    module Flowbox.Serialization,
    Mode,
    SValue
) where

import qualified Flowbox.Data.Serialization     as Serialization
import           Flowbox.Graphics.Serialization ()
import           Flowbox.Prelude
import           Generated.Proto.Data.SValue    (SValue)
import           Generated.Proto.Mode.Mode      (Mode)



computeValue :: Serialization.Serializable a b => a -> Mode -> IO (Maybe SValue)
computeValue var mode = Serialization.toValue var mode
