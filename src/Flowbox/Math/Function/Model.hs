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
type Weight      = Float
type Angle       = Float
type Range x     = (x, x)

data FunctionModel x y = FunctionModel { _segments :: Map x (Maybe (FunctionSegment x y)) }

data FunctionSegment x y = ContinuousHybrid { _vertices :: Map x (FunctionControlPoint y) }
                 | Lambda           { _lambda   :: x -> y }
                 | Repeater         { _startsAt :: x
                                    , _function :: FunctionModel x y
                                    , _range    :: Range x
                                    }

data FunctionControlPoint y = FunctionControlPoint { _vertexY   :: y
                                   , _handleIn  :: Maybe FunctionHandle
                                   , _handleOut :: Maybe FunctionHandle
                                   } deriving (Show, Eq)

data FunctionHandle = FunctionHandle { _weight   :: Weight
                     , _angle    :: Angle
                     }
            deriving (Show, Eq)

data FunctionPoint x y = FunctionPoint { _pointX :: x
                       , _pointY :: y
                       }

makeLenses ''FunctionPoint
makeLenses ''FunctionHandle
makeLenses ''FunctionControlPoint
makeLenses ''FunctionSegment
makeLenses ''FunctionModel


hardJoint :: y -> FunctionControlPoint y
hardJoint y = FunctionControlPoint y Nothing Nothing
