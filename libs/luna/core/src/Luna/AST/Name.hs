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

import           Flowbox.Prelude
import qualified Data.Map        as Map
import           Data.Map        (Map)
import           Flowbox.Generics.Deriving.QShow
import           Data.String.Utils (join)
import           Data.List         (intersperse)


----------------------------------------------------------------------
-- Data types
----------------------------------------------------------------------

data Name = Name { _base :: String, _segments :: [Segment] }
          deriving (Show, Eq, Generic, Read, Ord)

data Segment = Token String
             | Hole
             deriving (Show, Eq, Generic, Read, Ord)

makeLenses ''Name
instance QShow Name

instance QShow Segment


single :: String -> Name
single = flip Name []

multi :: String -> [Segment] -> Name
multi = Name


isSingle :: Name -> Bool
isSingle = null . view segments


isMulti :: Name -> Bool
isMulti = not . isSingle

segmentShow :: Segment -> String
segmentShow name = case name of
    Token s -> s
    Hole    -> "_"

toStr :: Name -> String
toStr n = if isSingle n
    then n^.base
    else (n^.base) ++ (' ' : join " " (fmap segmentShow $ n^.segments))


unified :: Name -> String
unified n = if isSingle n
    then n^.base
    else (n^.base) ++ ('_' : join "_" (fmap segmentShow $ n^.segments))


-- close the definition, check if name holes are defined explicite
-- define Holes otherwise
close :: Name -> Name
close n@(Name base segments) = case Hole `elem` segments of
    True  -> n
    False -> case null segments of
        True  -> n
        False -> Name base $ (Hole : intersperse Hole segments)
