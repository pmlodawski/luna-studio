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
type Weight      = Double
type Angle       = Double
type Range x     = (x, x)

data Function x y = Function { _segments :: Map x (Maybe (Segment x y)) }

data Segment x y = ContinuousHybrid { _nodes    :: Map x (ControlPoint y) }
                 | Lambda           { _lambda   :: x -> y }
                 | Repeater         { _startsAt :: x
                                    , _function :: Function x y
                                    , _range    :: Range x
                                    }

data ControlPoint y = ControlPoint { _nodeY     :: y
                                   , _handleIn  :: Maybe Handle
                                   , _handleOut :: Maybe Handle
                                   } deriving (Show, Eq)

data Handle = Handle { _weight   :: Weight
                     , _angle    :: Angle
                     }
            deriving (Show, Eq)

data Point x y = Point { _pointX :: x
                       , _pointY :: y
                       }

makeLenses ''Point
makeLenses ''Handle
makeLenses ''ControlPoint
makeLenses ''Segment
makeLenses ''Function


hardJoint :: y -> ControlPoint y
hardJoint y = ControlPoint y Nothing Nothing
