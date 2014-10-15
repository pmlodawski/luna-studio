---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE ViewPatterns           #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE FlexibleInstances      #-}

module Flowbox.Math.Function.Serialization where

import Flowbox.Graphics.Prelude

import Control.Monad
import Data.Map      hiding (fromList)
import Data.Maybe    (fromMaybe)
import Data.Sequence (fromList)

import           Flowbox.Math.Function.Model
import           Flowbox.Data.Serialization       (Serializable (..), mkValue)
import           Generated.Proto.Data.PointData   as PointData
import           Generated.Proto.Data.TangentData as TangentData
import           Generated.Proto.Data.VertexData  as VertexData
import           Generated.Proto.Data.CurveData   as CurveData
import qualified Generated.Proto.Data.Value       as Value
import qualified Generated.Proto.Data.Value.Type  as Value



instance (Real x, Real y) => Serializable (Point x y) PointData where
    serialize (Point (realToFrac -> x :: Double) (realToFrac -> y :: Double)) =
        return . Just $ PointData (Just x) (Just y)
    toValue a = liftM (mkValue PointData.data' Value.Point) $ serialize a

instance Serializable (Maybe Handle) TangentData where
    serialize mh  = return . Just $ TangentData w' a' b'
        where (w', a', b') = case mh of
                                 Nothing             -> (Just 0, Just 0, Just True)
                                 (Just (Handle w a)) -> (Just w, Just a, Just False)
    toValue a = liftM (mkValue TangentData.data' Value.Tangent) $ serialize a

instance (Real x, Real y) => Serializable (x, ControlPoint y) VertexData where
    serialize (realToFrac -> x :: Double, ControlPoint (realToFrac -> y :: Double) hIn hOut) = do
        p     <- serialize (Point x y)
        hIn'  <- serialize hIn
        hOut' <- serialize hOut
        return . Just $ VertexData p hIn' hOut'
    toValue a = liftM (mkValue VertexData.data' Value.Vertex) $ serialize a

instance (Real x, Real y) =>  Serializable (Segment x y) CurveData where
    serialize Lambda{}   = error "Serializing the Lambda segment is not yet supported!"
    serialize Repeater{} = error "Serializing the Repeater segment is not yet supported!"
    serialize (ContinuousHybrid nodes) = do
        nodes' <- sequence $ serialize <$> toAscList nodes
        return . Just . CurveData . fromList $ fromMaybe err $ sequence nodes'
        where err = error "Something went terribly wrong! Probably serialization of one of th evertices failed!"
    toValue a = liftM (mkValue CurveData.data' Value.Curve) $ serialize a
