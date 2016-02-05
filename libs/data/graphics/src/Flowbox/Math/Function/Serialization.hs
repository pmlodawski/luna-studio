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

module Flowbox.Math.Function.Serialization where

import           Flowbox.Graphics.Prelude         hiding (fromList)

import           Control.Monad
import           Data.Map                         hiding (fromList)
import           Data.Maybe                       (fromMaybe)
import           Data.Sequence                    (fromList)

import           Flowbox.Data.Serialization       (Serializable (..), mkValue)
import           Flowbox.Math.Function.Model
import           Generated.Proto.Data.CurveData   (CurveData (CurveData))
import qualified Generated.Proto.Data.CurveData   as CurveData
import           Generated.Proto.Data.PointData   (PointData (PointData))
import qualified Generated.Proto.Data.PointData   as PointData
import qualified Generated.Proto.Data.SValue.Type as SValue
import           Generated.Proto.Data.TangentData (TangentData (TangentData))
import qualified Generated.Proto.Data.TangentData as TangentData
import           Generated.Proto.Data.VertexData  (VertexData (VertexData))
import qualified Generated.Proto.Data.VertexData  as VertexData



instance (Real x, Real y) => Serializable (FunctionPoint x y) PointData where
    serialize (FunctionPoint x y) _ =
        return . Just $ PointData (Just . realToFrac $ x) (Just . realToFrac $ y)
    toValue a mode = liftM (mkValue PointData.data' SValue.Point) $ serialize a mode

instance Serializable (Maybe FunctionHandle) TangentData where
    serialize mh _ = return . Just $ TangentData (realToFrac <$> w') (realToFrac <$> a') b'
        where (w', a', b') = case mh of
                                 Nothing                     -> (Just 0, Just 0, Just True)
                                 (Just (FunctionHandle w a)) -> (Just w, Just a, Just False)
    toValue a mode = liftM (mkValue TangentData.data' SValue.Tangent) $ serialize a mode

instance (Real x, Real y) => Serializable (x, FunctionControlPoint y) VertexData where
    serialize (realToFrac -> x :: Float, FunctionControlPoint (realToFrac -> y :: Float) hIn hOut) mode = do
        p     <- serialize (FunctionPoint x y) mode
        hIn'  <- serialize hIn         mode
        hOut' <- serialize hOut        mode
        return . Just $ VertexData p hIn' hOut'
    toValue a mode = liftM (mkValue VertexData.data' SValue.Vertex) $ serialize a mode

instance (Real x, Real y) =>  Serializable (FunctionSegment x y) CurveData where
    serialize Lambda{}   _= error "Serializing the Lambda segment is not yet supported!"
    serialize Repeater{} _= error "Serializing the Repeater segment is not yet supported!"
    serialize (ContinuousHybrid nodes) mode = do
        nodes' <- sequence $ flip serialize mode <$> toAscList nodes
        return . Just . CurveData . fromList $ fromMaybe err $ sequence nodes'
        where err = error "Something went terribly wrong! Probably serialization of one of the vertices failed!"
    toValue a mode = liftM (mkValue CurveData.data' SValue.Curve) $ serialize a mode
