---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE TemplateHaskell #-}

module Luna.Syntax.Graph.Node.MultiPart where

import           Control.Monad.State
import qualified Data.Maybe          as Maybe

import           Flowbox.Prelude          hiding (mapM)
import qualified Luna.Syntax.Name.Pattern as Pattern



data MultiPartExpr base = MultiPartExpr
    { _prefix   :: Bool
    , _base     :: MultiPartSegment base
    , _segments :: [MultiPartSegment Pattern.SegmentName]
    } deriving (Show, Eq, Read)


data MultiPartSegment base = MultiPartSegment
    { _segmentBase :: base
    , _argsCount   :: Int
    } deriving (Show, Eq, Read)


makeLenses ''MultiPartExpr
makeLenses ''MultiPartSegment


fromNamePat :: Pattern.NamePat base arg -> MultiPartExpr base
fromNamePat (Pattern.NamePat nPrefix nBase nSegmentList) =
    MultiPartExpr (Maybe.isJust nPrefix)
                  (fromSegment nBase)
                  (map fromSegment nSegmentList)


fromSegment :: Pattern.Segment base arg -> MultiPartSegment base
fromSegment (Pattern.Segment nBase nSegmentArgs) =
    MultiPartSegment nBase $ length nSegmentArgs


toNamePat :: MultiPartExpr base -> [arg] -> arg -> Pattern.NamePat base arg
toNamePat (MultiPartExpr mPrefix mBase mSegments) args missing =
    flip evalState (args ++ repeat missing) $ do
        Pattern.NamePat <$> (if mPrefix then Just <$> arg else return Nothing)
                        <*> toSegment mBase
                        <*> mapM toSegment mSegments
    where
        toSegment (MultiPartSegment mBase mCount) = Pattern.Segment mBase <$> replicateM mCount arg
        arg = do
            h:t <- get
            put t
            return h
