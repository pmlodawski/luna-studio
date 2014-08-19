---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE TemplateHaskell #-}

module Flowbox.Luna.Data.Graph.Edge where

import           Flowbox.Luna.Data.Graph.Port (InPort, OutPort)
import qualified Flowbox.Luna.Data.Graph.Port as Port
import           Flowbox.Prelude



data Edge = Data { _src :: OutPort
                 , _dst :: InPort
                 }
          | Monadic
          deriving (Show, Read, Ord, Eq)


makeLenses(''Edge)


isData :: Edge -> Bool
isData (Data {}) = True
isData _         = False


isMonadic :: Edge -> Bool
isMonadic Monadic = True
isMonadic _       = False


match :: Edge -> Edge -> Bool
match (Data _ d) (Data s _) = Port.Num d == s
match  Monadic    Monadic   = True
match  _          _         = False
