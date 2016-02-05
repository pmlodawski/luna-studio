---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}

module Flowbox.Graphics.Shader.Sampler where

import           Flowbox.Graphics.Composition.Filter (Filter (..), convolve)
import           Flowbox.Graphics.Shader.Shader
import           Flowbox.Graphics.Utils.Utils
import           Flowbox.Math.Matrix                 as M hiding (get, size)
import           Flowbox.Prelude                     as P hiding (filter, transform)

import qualified Data.Array.Accelerate               as A
import           Math.Coordinate.Cartesian           (Point2 (..))



type Sampler e = ContinuousShader (Exp e) -> DiscreteShader (Exp e)

monosampler :: (Elt a, IsNum a) => CartesianShader (Exp a) e -> DiscreteShader e
monosampler = transform $ fmap A.fromIntegral

multisampler :: (Elt e, IsNum e, IsFloating e) => Matrix2 e -> CartesianShader (Exp e) (Exp e) -> DiscreteShader (Exp e)
multisampler kernel = convolve msampler kernel
    where fi = fmap A.fromIntegral
          msampler point offset = fi point + subpixel * fi offset
          Z :. h :. w = A.unlift $ shape kernel
          subpixel = Point2 (1 / (A.fromIntegral w - 1)) (1 / (A.fromIntegral h - 1))

nearest :: (Elt a, Elt e, IsFloating a) => DiscreteShader (Exp e) -> CartesianShader (Exp a) (Exp e)
nearest = transform $ fmap A.floor

interpolator :: forall e . (Elt e, IsFloating e) => Filter (Exp e) -> DiscreteShader (Exp e) -> CartesianShader (Exp e) (Exp e)
interpolator filter (Shader cnv input) = Shader cnv $ \pos@(Point2 x y) ->
    let get x' y' = input $ fmap A.floor pos + Point2 x' y'
        size = A.floor $ window filter
        kernel dx dy = apply filter (A.fromIntegral dx - frac x) * apply filter (A.fromIntegral dy - frac y)

        test :: (Exp Int, Exp e, Exp e) -> Exp Bool
        test (e, _, _) = e A.<=* size

        start :: Exp e -> Exp e -> Exp (Int, e, e)
        start val wei = A.lift (-size :: Exp Int, val :: Exp e, wei :: Exp e)

        outer :: (Exp Int, Exp e, Exp e) -> (Exp Int, Exp e, Exp e)
        outer (h, accV, accW) = (h + 1, resV, resW)
            where (_ :: Exp Int, resV :: Exp e, resW :: Exp e) = A.unlift $ A.while (A.lift1 test) (A.lift1 inner) (start accV accW)
                  inner :: (Exp Int, Exp e, Exp e) -> (Exp Int, Exp e, Exp e)
                  inner (w, accV', accW') = (w + 1, accV' + kernel w h * get w h, accW' + kernel w h)
        (_ :: Exp Int, valueSum :: Exp e, weightSum :: Exp e) = A.unlift $ A.while (A.lift1 test) (A.lift1 outer) (start 0 0)
    in valueSum / weightSum
