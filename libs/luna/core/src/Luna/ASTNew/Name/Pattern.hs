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
import           Luna.ASTNew.Name.Path (NamePath(NamePath))

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


--data NamePattern2 arg = NamePattern2 { _prefix   :: Maybe arg
--                                     , _base     :: Segment2 SegmentName arg
--                                     , _segments :: [Segment2 SegmentName arg]
--                                     }

--data Segment2 base arg = Segment2 base [arg]


--data ArgPattern a e = ArgPattern (LPat a) (Maybe e) 


single :: String -> NamePattern
single = flip NamePattern []

multi :: String -> [Segment] -> NamePattern
multi = NamePattern


toName :: NamePattern -> NamePath
toName (NamePattern base segments) = NamePath base $ reverse $ go [] segments
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