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
import qualified Flowbox.Graphics.Serialization as Serialization
import           Flowbox.Prelude
import           Generated.Proto.Data.Value     (Value)
import           Generated.Proto.Mode.Mode      (Mode)


type SValue = Value

computeValue :: Serialization.Serializable a b => a -> Mode -> IO SValue
computeValue var mode = Serialization.toValue var mode
