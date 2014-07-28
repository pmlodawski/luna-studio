---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}

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
        => U.Range (Exp t) -> Exp t -> Exp t
        -> U.Range (Exp t) -> Exp t -> Exp t
        -> U.Range (Exp t) -> Exp t -> Exp t
        -> a (Exp t)
        -> a (Exp t)
hsvTool hueRange hueRotation hueRolloff
        saturationRange saturationAdjustment saturationRolloff
        brightnessRange brightnessAdjustment brightnessRolloff pix =
    A.unlift (conditionsFulfilled A.? (
        --A.lift (hsv & h %~ (\hue -> rotation (hueRotation * rolloff hueRange hueRolloff hue) hue) -- hue
        --    & s %~ (\saturation -> saturation + saturationAdjustment * rolloff saturationRange saturationRolloff saturation)
        --    & v %~ (\value -> value + brightnessAdjustment * rolloff brightnessRange brightnessRolloff value)
        --    & convertColor :: a (Exp t))
        A.lift (hsv & h %~ (\hue -> power hueRange hueRolloff hue) -- rotation (hueRotation * power hueRange hueRolloff hue) hue) -- hue
            -- & s %~ (\saturation -> saturation + saturationAdjustment * power saturationRange saturationRolloff saturation)
            -- & v %~ (\value -> value + brightnessAdjustment * power brightnessRange brightnessRolloff value)
            & convertColor :: a (Exp t))
        ,
        A.lift pix))
    where hsv = toHSV pix

          --conditionsFulfilled = rolloff hueRange        hueRolloff        (hsv ^. h) A.>* 0
          --                      A.&&*
          --                      rolloff saturationRange saturationRolloff (hsv ^. s) A.>* 0
          --                      A.&&*
          --                      rolloff brightnessRange brightnessRolloff (hsv ^. v) A.>* 0
          --conditionsFulfilled = power hueRange        hueRolloff        (hsv ^. h) A.>* 0
          --                      A.&&*
          --                      power saturationRange saturationRolloff (hsv ^. s) A.>* 0
          --                      A.&&*
          --                      power brightnessRange brightnessRolloff (hsv ^. v) A.>* 0
          conditionsFulfilled = 1 A.>* (0::Exp Int)

rotation r hue = A.cond (hue' A.<* 0) (hue' + 1)
               $ A.cond (hue' A.>* 1) (hue' - 1)
               $ hue'
    where hue' = hue + r

inRange :: (IsScalar t, Elt t) => Exp t -> U.Range (Exp t) -> Exp Bool
inRange val (U.Range low high) = val A.>=* low A.&&* val A.<=* high

rolloff :: (Elt a, IsFloating a) => U.Range (Exp a) -> Exp a -> Exp a -> Exp a
rolloff range@(U.Range lo hi) roll val' = A.cond (val `inRange` range)   1
                                        $ A.cond (roll A.==* 0)          0
                                        $ A.cond (val A.<=* (lo - roll)) 0
                                        $ A.cond (val A.>=* (hi + roll)) 1
                                        $ A.cond (val A.<*  lo)          ((1 / roll) * val + 1 - (lo / roll))
                                        $ (((-1) / roll) * val + 1 + (hi / roll)) -- covers val > hi
    where val = val' + hi


power :: forall a. (Elt a, IsFloating a) => U.Range (Exp a) -> Exp a -> Exp a -> Exp a
power (U.Range a b) r x =
    let (correct, pp) = intersection a b r :: (Exp Bool, Exp a)
        (rLeft, rRight) = A.unlift (correct A.? (A.lift ((pp-1,a), (b,pp))
                                     , A.lift ((a-r, a), (b, b+r)))) :: (A.Exp (a, a), A.Exp (a, a))
        rLeftEquation val = A.cond (frL A./=* 0) (fxL val / frL) 1
        rRightEquation val = A.cond (frR A./=* 0) (1 - fxR val / frR) 1

        fxL val = U.frac $ val - tL
        frL = U.frac $ a - tL

        fxR val = U.frac $ val - tR
        frR = U.frac $ r -- == b+r - tR == b+r - b == r

        tL = U.frac $ a - r
        tR = U.frac $ b
    in check x (a,b) A.? (1,
       check x (A.unlift rLeft) A.? (rLeftEquation x,
       check x (A.unlift rRight) A.? (rRightEquation x, 0.6666)))

intersection :: (A.Elt a, A.IsFloating a) => Exp a -> Exp a -> Exp a -> (Exp Bool, Exp a)
intersection a b r = (y A.>* 0 A.&&* y A.<* 1, x)
    where x = (b2 - b1) / (a1 - a2)
          y = a1*((b2-b1)/(a1-a2))+b1
          (a1, b1) = rise a r
          (a2, b2) = fall b r

rise a r = (1/r, 1-(a+1)/r)

fall b r = ((-1)/r, 1+(b/r))

check :: (A.Elt a, A.IsFloating a) => Exp a -> (Exp a, Exp a) -> Exp Bool
check (U.frac -> x) (a,b) =
    let a' = U.frac a
        b' = U.frac b
    in (b-a A.>=* 1)
       A.||*
       (x A.>=* a' A.&&* x A.<=* b')
       A.||*
       (b' A.<* a' A.&&* ((x A.>=* a' A.&&* x A.>=* b')
           A.||*
           (x A.<=* a' A.&&* x A.<=* b')))



conditionally :: (Elt a, IsScalar a) => U.Range (Exp a) -> (Exp a -> Exp a) -> Exp a -> Exp a
conditionally (U.Range low high) f val = val A.>=* low A.&&* val A.<=* high A.? (f val, val)