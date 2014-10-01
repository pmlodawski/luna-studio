---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE TemplateHaskell #-}

module Luna.Graph.Edge where

import Flowbox.Prelude
import Luna.Graph.Port (Port)



data Edge = Data { _src :: Port
                 , _dst :: Port
                 }
          | Monadic
          deriving (Show, Read, Ord, Eq)


makeLenses ''Edge


isData :: Edge -> Bool
isData (Data {}) = True
isData _         = False


isMonadic :: Edge -> Bool
isMonadic Monadic = True
isMonadic _       = False


match :: Edge -> Edge -> Bool
match (Data _ d) (Data s _) = d == s
match  Monadic    Monadic   = True
match  _          _         = False
