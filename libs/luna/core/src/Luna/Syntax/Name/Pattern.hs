---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------

{-# LANGUAGE DeriveGeneric             #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FunctionalDependencies    #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TemplateHaskell           #-}

module Luna.Syntax.Name.Pattern where

import           Data.Binary                     (Binary)
import           Data.Foldable                   (Foldable)
import           Data.List                       (intersperse)
import           Data.Map                        (Map)
import qualified Data.Map                        as Map
import           Data.Maybe                      (isNothing)
import           Data.String                     (IsString, fromString)
import           Data.String.Utils               (join)
import           GHC.Generics                    (Generic)

import           Flowbox.Generics.Deriving.QShow
import           Flowbox.Prelude
import           Luna.Syntax.Arg                 (Arg (Arg))
import           Luna.Syntax.Name.Hash           (Hashable, hash)
import           Luna.Syntax.Name.Path           (NamePath (NamePath))

----------------------------------------------------------------------
-- Type classes
----------------------------------------------------------------------

class NamePatternClass p s | p -> s where
    segments     :: p -> [s]
    toNamePath   :: p -> NamePath

class SegName s n | s -> n where
    segName :: s ->  n

----------------------------------------------------------------------
-- Data types
----------------------------------------------------------------------

data NamePat base arg = NamePat { _prefix      :: Maybe arg
                                , _base        :: Segment base arg
                                , _segmentList :: [Segment SegmentName arg]
            -- FIXME [kgdk -> wd]: ^-- better name for this. Was '_segments', but this name conflicts
            -- with method in NamePatternClass
                                } deriving (Show, Eq, Generic, Read, Ord, Functor, Traversable, Foldable)

data Segment base arg = Segment { _segmentBase :: base
                                , _segmentArgs :: [arg]
            -- FIXME [kgdk -> wd]: ^-- better name for this (more conflicts). Long-term solution:
            -- refactor to separate file
                                } deriving (Show, Eq, Generic, Read, Ord, Functor, Traversable, Foldable)
type ArgPat  a v      = NamePat     SegmentName (Arg a v)
data NamePatDesc      = NamePatDesc  Bool SegmentDesc [SegmentDesc] deriving (Show, Eq, Generic, Read, Ord)
data SegmentDesc      = SegmentDesc SegmentName [Bool]             deriving (Show, Eq, Generic, Read, Ord)

type SegmentName      = Text


makeLenses ''NamePat
makeLenses ''Segment

----------------------------------------------------------------------
-- Utils
----------------------------------------------------------------------

single :: base -> [arg] -> NamePat base arg
single base args = NamePat Nothing (Segment base args) []

appendArg :: arg -> Segment base arg -> Segment base arg
appendArg  arg  = appendArgs [arg]

appendArgs :: [arg] -> Segment base arg -> Segment base arg
appendArgs nargs (Segment base args) = Segment base (args ++ nargs)

appendLastSegmentArg :: arg -> NamePat base arg -> NamePat base arg
appendLastSegmentArg  arg  = appendLastSegmentArgs [arg]

appendLastSegmentArgs :: [arg] -> NamePat base arg -> NamePat base arg
appendLastSegmentArgs nargs (NamePat pfx base segs) = case segs of
    [] -> NamePat pfx (appendArgs nargs base) segs
    _  -> NamePat pfx base (init segs ++ [appendArgs nargs $ last segs])

withLastSegment :: (Segment SegmentName arg -> Segment SegmentName arg)
                ->  NamePat SegmentName arg -> NamePat SegmentName arg
withLastSegment f (NamePat pfx base segs) = case segs of
    [] -> NamePat pfx (f base) segs
    _  -> NamePat pfx base (init segs ++ [f $ last segs])

toDesc :: ArgPat a v -> NamePatDesc
toDesc (NamePat pfx base segments) = NamePatDesc (pfxToDesc pfx) (smntToDesc base) (fmap smntToDesc segments)
    where smntToDesc (Segment base args) = SegmentDesc base $ fmap argToDesc args
          argToDesc  (Arg _ e)           = isNothing e
          pfxToDesc  (Just arg)          = argToDesc arg
          pfxToDesc  Nothing             = True

mapSegmentBase :: (t -> base) -> Segment t arg -> Segment base arg
mapSegmentBase f (Segment base arg) = Segment (f base) arg

mapSegments :: (Segment SegmentName arg -> Segment SegmentName arg)
            ->  NamePat SegmentName arg -> NamePat SegmentName arg
mapSegments f (NamePat pfx base segs) = NamePat pfx (f base) (fmap f segs)

segArgs :: Segment base arg -> [arg]
segArgs (Segment _ args) = args

segBase :: Segment base arg -> base
segBase (Segment base _) = base

args :: NamePat base arg -> [arg]
args (NamePat pfx base segs) = allArgs
    where postArgs = segArgs base ++ concat (fmap segArgs segs)
          allArgs  = case pfx of
              Just a  -> a : postArgs
              Nothing -> postArgs

segmentNames = fmap segName . segments


----------------------------------------------------------------------
-- Instances
----------------------------------------------------------------------

instance NamePatternClass (NamePat SegmentName arg) (Segment SegmentName arg) where
    segments (NamePat _ base segs) = base : segs
    toNamePath (NamePat _ base segs) = NamePath (sgmtName base) (fmap sgmtName segs)
        where sgmtName (Segment base _) = base

instance NamePatternClass NamePatDesc SegmentDesc where
    segments (NamePatDesc _ base segs) = base : segs
    toNamePath (NamePatDesc _ base segs) = NamePath (sgmtName base) (fmap sgmtName segs)
        where sgmtName (SegmentDesc base _) = base

instance Hashable base Text => Hashable (NamePat base arg) Text where
    hash (NamePat _ base segs) = mjoin "_" $ segNames
        where fSegment f (Segment base _) = f base
              segNames = fSegment hash base : fmap (fSegment hash) segs


instance SegName (Segment base arg) base where
    segName (Segment base _) = base

instance SegName SegmentDesc SegmentName where
    segName (SegmentDesc base _) = base

instance (Binary base, Binary arg) => Binary (NamePat base arg)
instance (Binary base, Binary arg) => Binary (Segment base arg)
