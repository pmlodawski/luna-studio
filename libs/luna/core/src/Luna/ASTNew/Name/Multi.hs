---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------

{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TemplateHaskell           #-}
{-# LANGUAGE DeriveGeneric             #-}


module Luna.ASTNew.Name.Multi where


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

data MultiName = MultiName { _base :: String, _segments :: [Segment] }
          deriving (Show, Eq, Generic, Read, Ord)

data Segment = Token String
             | Hole
             deriving (Show, Eq, Generic, Read, Ord)

makeLenses ''MultiName
instance QShow MultiName
instance QShow Segment


single :: String -> MultiName
single = flip MultiName []

multi :: String -> [Segment] -> MultiName
multi = MultiName


isSingle :: MultiName -> Bool
isSingle = null . view segments


isMulti :: MultiName -> Bool
isMulti = not . isSingle

segmentShow :: Segment -> String
segmentShow name = case name of
    Token s -> s
    Hole    -> "_"

toStr :: MultiName -> String
toStr n = if isSingle n
    then n^.base
    else (n^.base) ++ (' ' : join " " (fmap segmentShow $ n^.segments))


unified :: MultiName -> String
unified n = if isSingle n
    then n^.base
    else (n^.base) ++ ('_' : join "_" (fmap segmentShow $ n^.segments))


-- close the definition, check if name holes are defined explicite
-- define Holes otherwise
close :: MultiName -> MultiName
close n@(MultiName base segments) = case Hole `elem` segments of
    True  -> n
    False -> case null segments of
        True  -> n
        False -> MultiName base $ (Hole : intersperse Hole segments)
