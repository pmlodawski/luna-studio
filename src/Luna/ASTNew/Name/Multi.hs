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

data MultiName a = MultiName { _base :: a, _segments :: [Segment a] }
          deriving (Show, Eq, Generic, Read, Ord)

data Segment a = Token a
               | Hole
               deriving (Show, Eq, Generic, Read, Ord)

makeLenses ''MultiName
instance QShow a => QShow (MultiName a)
instance QShow a => QShow (Segment a)


single :: a -> MultiName a
single = flip MultiName []

multi :: a -> [Segment a] -> MultiName a
multi = MultiName


isSingle :: MultiName a -> Bool
isSingle = null . view segments


isMulti :: MultiName a -> Bool
isMulti = not . isSingle

segmentShow :: StrRepr a => Segment a -> String
segmentShow name = case name of
    Token s -> strRepr s
    Hole    -> "_"

toStr :: StrRepr a => MultiName a -> String
toStr n = if isSingle n
    then strRepr $ n^.base
    else (strRepr $ n^.base) ++ (' ' : join " " (fmap segmentShow $ n^.segments))


unified :: StrRepr a => MultiName a -> String
unified n = if isSingle n
    then strRepr $ n^.base
    else (strRepr $ n^.base) ++ ('_' : join "_" (fmap segmentShow $ n^.segments))


-- close the definition, check if name holes are defined explicite
-- define Holes otherwise
close :: Eq a => MultiName a -> MultiName a
close n@(MultiName base segments) = case Hole `elem` segments of
    True  -> n
    False -> case null segments of
        True  -> n
        False -> MultiName base $ (Hole : intersperse Hole segments)
