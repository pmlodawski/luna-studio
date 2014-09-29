---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
module Flowbox.Math.Function.Model where

import Data.Map

import Flowbox.Prelude



-- | Helper types
type CoordinateX = Double
type CoordinateY = Double
type Weight      = Double
type Angle       = Double

data Handle = Handle { _weight :: Weight
                     , _angle  :: Angle
                     }
            | EmptyHandle
            deriving (Show, Eq)

makeLenses ''Handle

-- INFO: might be obsolete # decide whether this should be a constructor or defined like this
emptyHandle :: Handle
emptyHandle = Handle 0 0

data Node = Node { _nodeY     :: CoordinateY
                 , _handleIn  :: Handle
                 , _handleOut :: Handle
                 } deriving (Show, Eq)

makeLenses ''Node

data Segment = BSpline { _nodes  :: Map CoordinateX Node }
             | Lambda  { _lambda :: CoordinateX -> CoordinateY }
             | EmptySegment
             --deriving (Show, Eq)

makeLenses ''Segment

data Function = Function { _firstSegment :: Segment
                         , _segments     :: Map CoordinateX Segment
                         } --deriving (Show, Eq)

makeLenses ''Function
