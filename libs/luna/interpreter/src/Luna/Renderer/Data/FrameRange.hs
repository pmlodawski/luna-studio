---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Flowbox Team <contact@flowbox.io>, 2014
-- Proprietary and confidential
-- Unauthorized copying of this file, via any medium is strictly prohibited
---------------------------------------------------------------------------
{-# LANGUAGE TemplateHaskell #-}
module Luna.Renderer.Data.FrameRange where

import           Data.IntSet                        (IntSet)
import qualified Data.IntSet                        as IntSet

import           Flowbox.Prelude
import           Luna.Interpreter.Session.Data.Time (Time)



data FrameRange = FrameRange { _begin :: Time
                             , _end   :: Time
                             } deriving (Show)

makeLenses ''FrameRange


type FrameRanges = [FrameRange]


frames :: FrameRanges -> IntSet
frames = IntSet.unions . map frames' where
    frames' (FrameRange b e) = IntSet.fromList $ enumFromTo (ceiling b) (floor e)
