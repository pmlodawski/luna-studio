---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------

{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TemplateHaskell           #-}
{-# LANGUAGE DeriveGeneric             #-}
{-# LANGUAGE FunctionalDependencies    #-}


module Luna.ASTNew.Name.Pattern2 where


import GHC.Generics (Generic)

import           Flowbox.Prelude
import qualified Data.Map        as Map
import           Data.Map        (Map)
import           Flowbox.Generics.Deriving.QShow
import           Data.String.Utils (join)
import           Data.List         (intersperse)
import           Data.String             (IsString, fromString)
import           Luna.ASTNew.Name.Path (NamePath(NamePath))
import           Data.Maybe (isNothing)
import           Data.Foldable (Foldable)

----------------------------------------------------------------------
-- Data types
----------------------------------------------------------------------

type SegmentName = String

data NamePat base arg = NamePat { _prefix   :: Maybe arg
                                , _base     :: Segment base arg
                                , _segments :: [Segment SegmentName arg]
                                } deriving (Show, Eq, Generic, Read, Ord, Functor, Traversable, Foldable)

data Segment base arg = Segment base [arg] deriving (Show, Eq, Generic, Read, Ord, Functor, Traversable, Foldable)


data Arg pat expr = Arg pat (Maybe expr) deriving (Show, Eq, Generic, Read, Ord)

type ArgPat pat expr = NamePat SegmentName (Arg pat expr)

data SegmentDesc = SegmentDesc SegmentName [Bool] deriving (Show, Eq, Generic, Read, Ord)

data ArgPatDesc = ArgPatDesc Bool SegmentDesc [SegmentDesc] deriving (Show, Eq, Generic, Read, Ord)


appendArg  arg  = appendArgs [arg]
appendArgs nargs (Segment base args) = Segment base (args ++ nargs)

appendLastSegmentArg  arg  = appendLastSegmentArgs [arg]
appendLastSegmentArgs nargs (NamePat pfx base segs) = case segs of
    [] -> NamePat pfx (appendArgs nargs base) segs
    _  -> NamePat pfx base (init segs ++ [appendArgs nargs $ last segs])


withLastSegment f (NamePat pfx base segs) = case segs of
    [] -> NamePat pfx (f base) segs
    _  -> NamePat pfx base (init segs ++ [f $ last segs])

toDesc :: ArgPat pat expr -> ArgPatDesc
toDesc (NamePat pfx base segments) = ArgPatDesc (pfxToDesc pfx) (smntToDesc base) (fmap smntToDesc segments)
    where smntToDesc (Segment base args) = SegmentDesc base $ fmap argToDesc args
          argToDesc  (Arg _ mexpr)       = isNothing mexpr
          pfxToDesc  (Just arg)          = argToDesc arg
          pfxToDesc  Nothing             = True


mapSegmentBase f (Segment base arg) = Segment (f base) arg

mapSegments f (NamePat pfx base segs) = NamePat pfx (f base) (fmap f segs)

class NamePatternClass p s | p -> s where
    segments     :: p -> [s]
    segmentNames :: p -> [SegmentName]
    toNamePath   :: p -> NamePath

instance NamePatternClass (NamePat SegmentName arg) (Segment SegmentName arg) where
    segments (NamePat _ base segs) = base : segs
    segmentNames = fmap sgmtName . segments 
        where sgmtName (Segment base _) = base
    toNamePath (NamePat _ base segs) = NamePath (sgmtName base) (fmap sgmtName segs)
        where sgmtName (Segment base _) = base

instance NamePatternClass ArgPatDesc SegmentDesc where
    segments (ArgPatDesc _ base segs) = base : segs 
    segmentNames = fmap sgmtName . segments 
        where sgmtName (SegmentDesc base _) = base
    toNamePath (ArgPatDesc _ base segs) = NamePath (sgmtName base) (fmap sgmtName segs)
        where sgmtName (SegmentDesc base _) = base

--instance QShow (NamePat)
--instance QShow (Segment)




----------------------------------------------------------------------
-- Instances
----------------------------------------------------------------------
