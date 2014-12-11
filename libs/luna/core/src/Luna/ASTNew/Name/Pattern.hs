---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------

{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TemplateHaskell           #-}
{-# LANGUAGE DeriveGeneric             #-}


module Luna.ASTNew.Name.Pattern where


import GHC.Generics (Generic)

import           Flowbox.Prelude
import qualified Data.Map        as Map
import           Data.Map        (Map)
import           Flowbox.Generics.Deriving.QShow
import           Data.String.Utils (join)
import           Data.List         (intersperse)
import           Data.String             (IsString, fromString)
import           Luna.ASTNew.Name.Multi (MultiName(MultiName))

----------------------------------------------------------------------
-- Data types
----------------------------------------------------------------------

data NamePattern = NamePattern String [Segment]
          deriving (Show, Eq, Generic, Read, Ord)

data Segment = Token String
             | Hole
             deriving (Show, Eq, Generic, Read, Ord)

instance QShow (NamePattern)
instance QShow (Segment)


single :: String -> NamePattern
single = flip NamePattern []

multi :: String -> [Segment] -> NamePattern
multi = NamePattern


toName :: NamePattern -> MultiName
toName (NamePattern base segments) = MultiName base $ reverse $ go [] segments
    where go x []           = x
          go x (Hole:xs)    = go x xs
          go x (Token n:xs) = go (n:x) xs

-- close the definition, check if name holes are defined explicite
-- define Holes otherwise
close :: NamePattern -> NamePattern
close n@(NamePattern base segments) = case Hole `elem` segments of
    True  -> n
    False -> case null segments of
        True  -> n
        False -> NamePattern base $ (Hole : intersperse Hole segments)


----------------------------------------------------------------------
-- Instances
----------------------------------------------------------------------

instance IsString NamePattern  where fromString = single