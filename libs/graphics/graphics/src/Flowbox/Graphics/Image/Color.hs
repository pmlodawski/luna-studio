---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Flowbox.Graphics.Image.Color (
    module Flowbox.Graphics.Image.Color,
    U.Range(..),
    U.bias,
    U.clamp',
    U.clamp,
    U.gain,
    U.gamma,
    U.invert,
    U.mix
) where

import qualified Data.Array.Accelerate as A

import           Flowbox.Graphics.Color
import qualified Flowbox.Graphics.Utils as U
import           Flowbox.Math.Matrix    as M
import           Flowbox.Prelude        as P



offset :: (Elt a, IsNum a) => Exp a -> Exp a -> Exp a
offset v = (+v)

multiply :: (Elt a, IsNum a) => Exp a -> Exp a -> Exp a
multiply v = (*v)

contrast :: (Elt a, IsFloating a) => Exp a -> Exp a -> Exp a
contrast v x = (x - 0.5) * v + 0.5

data Colorspace = Linear | Cineon

exposure :: (Elt a, IsFloating a) => Exp a -> Exp a -> Exp a -> Exp a
exposure blackpoint ex pix = blackpointConvert blackpoint (2 ** ex * (inverseBlackpointConvert blackpoint pix))

blackpointConvert :: (Elt a, IsFloating a) => Exp a -> Exp a -> Exp a
blackpointConvert blackpoint pix = pointsConvert blackpoint 1.0 pix

inverseBlackpointConvert :: (Elt a, IsFloating a) => Exp a -> Exp a -> Exp a
inverseBlackpointConvert lift pix = inversePointsConvert lift 1.0 pix

whitepointConvert :: (Elt a, IsFloating a) => Exp a -> Exp a -> Exp a
whitepointConvert whitepoint pix = pointsConvert 0.0 whitepoint pix

inverseWhitepointConvert :: (Elt a, IsFloating a) => Exp a -> Exp a -> Exp a
inverseWhitepointConvert gain pix = inversePointsConvert 0.0 gain pix

pointsConvert :: (Elt a, IsFloating a) => Exp a -> Exp a -> Exp a -> Exp a
pointsConvert blackpoint whitepoint pix = (pix - blackpoint) / (whitepoint - blackpoint)

inversePointsConvert :: (Elt a, IsFloating a) => Exp a -> Exp a -> Exp a -> Exp a
inversePointsConvert lift gain pix = (gain - lift) * pix + lift

grade :: (Elt a, IsFloating a) => Exp a -> Exp a -> Exp a -> Exp a -> Exp a -> Exp a -> Exp a -> Exp a -> Exp a
grade blackpoint whitepoint lift gain multiply' offset' gamma =
	U.gamma gamma . offset offset' . multiply multiply' . inversePointsConvert lift gain . pointsConvert blackpoint whitepoint

colorCorrect :: (Elt a, IsFloating a) => Exp a -> Exp a -> Exp a -> Exp a -> Exp a -> RGB (Exp a) -> RGB (Exp a)
colorCorrect saturation' contrast' gamma' gain' offset' pix =
    saturated & each %~ (offset offset' . gain'' gain' . U.gamma gamma' . contrast contrast')
    where saturated  = toHSV pix & s %~ (*saturation') & toRGB
          gain'' b x = b * x -- U.gain is broken, tested with Nuke that it's simply multiplication

-- NOTE[mm]: pretty sure Nuke uses HSL colorspace for saturation manipulation. There are slight differences still,
--           but operating on HSV looks unalike to Nuke.
saturate :: (Elt t, IsFloating t, ColorConvert a HSL, ColorConvert HSL a) => Exp t -> a (Exp t) -> a (Exp t)
saturate saturation pix = toHSL pix & s %~ (*saturation) & convertColor

hsvTool :: forall a t. (Elt t, IsFloating t, ColorConvert a HSV, ColorConvert HSV a,
                        A.Lift Exp (a (Exp t)), A.Unlift Exp (a (Exp t)), Elt (A.Plain (a (Exp t))))
        => U.Range (Exp t) -> Exp t
        -> U.Range (Exp t) -> Exp t
        -> U.Range (Exp t) -> Exp t
        -> a (Exp t)
        -> a (Exp t)
hsvTool hueRange hueRotation saturationRange saturationAdjustment brightnessRange brightnessAdjustment pix =
    A.unlift (conditionsFulfilled A.? (
        A.lift (hsv & h %~ rotation hueRotation
            & s %~ (+ saturationAdjustment)
            & v %~ (+ brightnessAdjustment)
            & convertColor :: a (Exp t))
        ,
        A.lift pix))
    where hsv = toHSV pix
          rotation r hue = A.cond (hue' A.<* 0) (hue' + 1)
                         $ A.cond (hue' A.>* 1) (hue' - 1)
                         $ hue'
              where hue' = hue + r / 360

          conditionsFulfilled = (hsv ^. h) `inRange` hueRange
                                A.&&*
                                (hsv ^. s) `inRange` saturationRange
                                A.&&*
                                (hsv ^. v) `inRange` brightnessRange

inRange :: (IsScalar t, Elt t) => Exp t -> U.Range (Exp t) -> Exp Bool
inRange val (U.Range low high) = val A.>=* low A.&&* val A.<=* high

conditionally :: (Elt a, IsScalar a) => U.Range (Exp a) -> (Exp a -> Exp a) -> Exp a -> Exp a
conditionally (U.Range low high) f val = val A.>=* low A.&&* val A.<=* high A.? (f val, val)