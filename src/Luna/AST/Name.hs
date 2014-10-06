---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------

{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TemplateHaskell           #-}

module Luna.AST.Name where


import GHC.Generics (Generic)

import Data.String.Utils               (join)
import Flowbox.Generics.Deriving.QShow
import Flowbox.Prelude


----------------------------------------------------------------------
-- Data types
----------------------------------------------------------------------

data Name = Name { _base :: String, _segments :: [String] }
          deriving (Show, Eq, Generic, Read, Ord)

makeLenses ''Name
instance QShow Name


single :: String -> Name
single = flip Name []

multi :: String -> [String] -> Name
multi = Name


isSingle :: Name -> Bool
isSingle = null . view segments


isMulti :: Name -> Bool
isMulti = not . isSingle


toStr :: Name -> String
toStr n = if isSingle n
    then n^.base
    else (n^.base) ++ (' ' : join " " (n^.segments))


unified :: Name -> String
unified n = if isSingle n
    then n^.base
    else (n^.base) ++ ('_' : join "_" (n^.segments))
