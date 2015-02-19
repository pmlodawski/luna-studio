---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE ViewPatterns          #-}

module Flowbox.Math.Function.Model where

import Data.Map      (Map, toAscList)
import Data.Maybe    (fromMaybe)
import Data.Sequence (fromList)

import           Flowbox.Data.Serialization       (Serializable(..))
import           Flowbox.Prelude
import           Generated.Proto.Data.CurveData   (CurveData (CurveData))
import qualified Generated.Proto.Data.CurveData   as CurveData
import           Generated.Proto.Data.PointData   (PointData (PointData))
import qualified Generated.Proto.Data.PointData   as PointData
import           Generated.Proto.Data.TangentData (TangentData (TangentData))
import qualified Generated.Proto.Data.TangentData as TangentData
import qualified Generated.Proto.Data.Value.Type  as Value
import           Generated.Proto.Data.VertexData  (VertexData (VertexData))
import qualified Generated.Proto.Data.VertexData  as VertexData



-- | Helper types
type Weight      = Double
type Angle       = Double
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


instance (Real x, Real y) => Serializable (FunctionPoint x y) PointData where
    serialize (FunctionPoint (realToFrac -> x :: Double) (realToFrac -> y :: Double)) _ =
        return . Just $ PointData (Just x) (Just y)
    data' _ = PointData.data'
    val   _ = Value.Point

instance Serializable (Maybe FunctionHandle) TangentData where
    serialize mh _ = return . Just $ TangentData w' a' b'
        where (w', a', b') = case mh of
                                 Just (FunctionHandle w a) -> (Just w, Just a, Just False)
                                 Nothing                   -> (Just 0, Just 0, Just True)
    data' _ = TangentData.data'
    val   _ = Value.Tangent

instance (Real x, Real y) => Serializable (x, FunctionControlPoint y) VertexData where
    serialize (realToFrac -> x :: Double, FunctionControlPoint (realToFrac -> y :: Double) hIn hOut) mode = do
        p     <- serialize (FunctionPoint x y) mode
        hIn'  <- serialize hIn         mode
        hOut' <- serialize hOut        mode
        return . Just $ VertexData p hIn' hOut'
    data' _ = VertexData.data'
    val   _ = Value.Vertex

instance (Real x, Real y) =>  Serializable (FunctionSegment x y) CurveData where
    serialize Lambda{}   _= error "Serializing the Lambda segment is not yet supported!"
    serialize Repeater{} _= error "Serializing the Repeater segment is not yet supported!"
    serialize (ContinuousHybrid nodes) mode = do
        nodes' <- sequence $ flip serialize mode <$> toAscList nodes
        return . Just . CurveData . fromList $ fromMaybe err $ sequence nodes'
        where err = error "Something went terribly wrong! Probably serialization of one of the vertices failed!"
    data' _ = CurveData.data'
    val   _ = Value.Curve
