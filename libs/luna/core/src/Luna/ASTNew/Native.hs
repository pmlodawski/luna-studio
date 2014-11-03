---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE DeriveGeneric #-}

module Luna.ASTNew.Native where


import Flowbox.Prelude
import GHC.Generics


data Native e = Code [NativeSegment]
              | AST e
              deriving (Show, Eq, Generic, Read)

data NativeSegment = Str { _code :: String }
                   | Var { _name :: String } 
                   deriving (Show, Eq, Generic, Read)