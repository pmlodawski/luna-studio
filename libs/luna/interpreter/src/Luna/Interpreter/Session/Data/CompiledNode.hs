---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE TemplateHaskell #-}

module Luna.Interpreter.Session.Data.CompiledNode where

import Data.HMap (HMap)

import Flowbox.Prelude
import Generated.Proto.Data.Value (Value)
import Generated.Proto.Mode.Mode  (Mode)



data CompiledNode = CompiledNode { _update :: HMap -> HMap
                                 , _value  :: Maybe (HMap -> Mode -> IO Value)
                                 }

makeLenses ''CompiledNode
