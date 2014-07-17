---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE TemplateHaskell #-}

module Flowbox.Luna.Data.Graph.Edge where

import Flowbox.Luna.Data.Graph.Port (InPort, OutPort)
import Flowbox.Prelude



data Edge = Edge { _src :: OutPort
                 , _dst :: InPort
                 } deriving (Show, Read, Ord, Eq)

makeLenses(''Edge)
