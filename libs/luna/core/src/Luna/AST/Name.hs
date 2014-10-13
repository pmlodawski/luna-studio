---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------

{-# LANGUAGE NoMonomorphismRestriction #-}

module Luna.AST.Name where


import GHC.Generics        (Generic)

import           Flowbox.Prelude
import qualified Data.Map        as Map
import           Data.Map        (Map)
import           Flowbox.Generics.Deriving.QShow
import           Data.String.Utils (join)
import           Data.List         (intersperse)


----------------------------------------------------------------------
-- Data types
----------------------------------------------------------------------

data Name = Name { _base :: String, _segments :: [NameSegment] }
          deriving (Show, Eq, Generic, Read)


data NameSegment = NameToken String
                 | NameHole
                 deriving (Show, Eq, Generic, Read)

makeLenses ''Name
instance QShow Name

instance QShow NameSegment


single = flip Name []
multi  = Name

isSingle :: Name -> Bool
isSingle = null . view segments

isMulti :: Name -> Bool
isMulti = not . isSingle

segmentShow :: NameSegment -> String
segmentShow name = case name of
    NameToken s -> s
    NameHole    -> "_"

toStr :: Name -> String
toStr n = if isSingle n
    then n^.base
    else (n^.base) ++ (' ' : join " " (fmap segmentShow $ n^.segments))


unified :: Name -> String
unified n = if isSingle n
    then n^.base
    else (n^.base) ++ ('_' : join "_" (fmap segmentShow $ n^.segments))


-- close the definition, check if name holes are defined explicite
-- define nameholes otherwise
close :: Name -> Name
close n@(Name base segments) = case NameHole `elem` segments of
    True  -> n
    False -> case null segments of
        True  -> n
        False -> Name base $ (NameHole : intersperse NameHole segments)
