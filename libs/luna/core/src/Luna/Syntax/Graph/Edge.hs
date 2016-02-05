---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE TemplateHaskell #-}

module Luna.Syntax.Graph.Edge where

import           Flowbox.Prelude
import           Luna.Syntax.Graph.Port (DstPort, SrcPort)



data Edge = Data { _src :: SrcPort
                 , _dst :: DstPort
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
match (Data _ d) (Data s _) = unwrap d == unwrap s
match  Monadic    Monadic   = True
match  _          _         = False
