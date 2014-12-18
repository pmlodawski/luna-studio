---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------

{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TemplateHaskell           #-}
{-# LANGUAGE DeriveGeneric             #-}


module Luna.ASTNew.Name.Path where


import GHC.Generics (Generic)

import           Flowbox.Prelude
import qualified Data.Map        as Map
import           Data.Map        (Map)
import           Flowbox.Generics.Deriving.QShow
import           Data.String.Utils (join)
import           Data.List         (intersperse)
import           Data.String             (IsString, fromString)
import           Luna.ASTNew.Name.Hash (Hashable, hash)


----------------------------------------------------------------------
-- Data types
----------------------------------------------------------------------

data NamePath = NamePath { _base :: String, _segments :: [String] }
          deriving (Show, Eq, Generic, Read, Ord)

data Segment = Token String
             | Hole
             deriving (Show, Eq, Generic, Read, Ord)

makeLenses ''NamePath
instance QShow (NamePath)
instance QShow (Segment)



toList :: NamePath -> [String]
toList (NamePath b s) = b:s

single :: String -> NamePath
single = flip NamePath []

multi :: String -> [String] -> NamePath
multi = NamePath


isSingle :: NamePath -> Bool
isSingle = null . view segments


isMulti :: NamePath -> Bool
isMulti = not . isSingle

segmentShow :: Segment -> String
segmentShow name = case name of
    Token s -> strRepr s
    Hole    -> "_"

toStr :: NamePath -> String
toStr n = if isSingle n
    then strRepr $ n^.base
    else (strRepr $ n^.base) ++ (' ' : join " " (n^.segments))


unified :: NamePath -> String
unified n = if isSingle n
    then strRepr $ n^.base
    else (strRepr $ n^.base) ++ ('_' : join "_" (n^.segments))


----------------------------------------------------------------------
-- Instances
----------------------------------------------------------------------

instance IsString NamePath  where fromString = single

instance Hashable NamePath where
	hash (NamePath base segs) = hash base ++ concat (fmap hash segs)

instance ToString NamePath where
    toString (NamePath base segs) = base <> mjoin " " segs