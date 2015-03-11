---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE ViewPatterns        #-}
{-# LANGUAGE ConstraintKinds      #-}

module Flowbox.Graphics.Composition.Color (
    module Flowbox.Graphics.Composition.Color,
    U.Range(..),
    U.bias,
    U.clamp',
    U.clamp,
    --U.gain, its broken!
    U.gamma,
    U.invert,
    U.mix
) where

import qualified Data.Array.Accelerate       as A
import           Data.Array.Accelerate.Tuple (IsTuple, TupleRepr, fromTuple, toTuple)
--import           Data.Array.Accelerate.Type  (IsScalar)

import           Flowbox.Graphics.Color.Color
import           Flowbox.Graphics.Shader.Shader
import           Flowbox.Graphics.Utils.Accelerate        (variable)
import qualified Flowbox.Graphics.Utils.Utils             as U
import           Flowbox.Math.Function.Accelerate.BSpline as BSpline
import           Flowbox.Math.Matrix                      as M
import           Flowbox.Graphics.Prelude                 as P

offset :: (Num a) => a -> a -> a
offset v = (+v)

multiply :: (Num a) => a -> a -> a
multiply v = (*v)

contrast :: (Num a, Fractional a, Floating a, Condition a, Ord a) => a -> a -> a
contrast v x = if' (v >= 0.0) (((x/0.18) ** v) * 0.18) ((0.18 ** v) * 0.18)
-- [NOTE] Changed so that it works just like in Nuke
--        Look here:
--        https://compositormathematic.wordpress.com/2013/07/06/gamma-contrast/
--        old version:
--        contrast v x = (x - 0.5) * v + 0.5 -- that's not how they do it in Nuke

data Colorspace = Linear | Cineon

exposure :: (Num a, Floating a) => a -> a -> a -> a
exposure blackpoint ex pix = blackpointConvert blackpoint (2 ** ex * inverseBlackpointConvert blackpoint pix)

blackpointConvert :: (Num a, Floating a) => a -> a -> a
blackpointConvert blackpoint = pointsConvert blackpoint 1.0

inverseBlackpointConvert :: (Num a, Floating a) => a -> a -> a
inverseBlackpointConvert lift = inversePointsConvert lift 1.0

whitepointConvert :: (Num a, Floating a) => a -> a -> a
whitepointConvert = pointsConvert 0.0

inverseWhitepointConvert :: (Num a, Floating a) => a -> a -> a
inverseWhitepointConvert = inversePointsConvert 0.0

pointsConvert :: (Num a, Floating a) => a -> a -> a -> a
pointsConvert blackpoint whitepoint pix = (pix - blackpoint) / (whitepoint - blackpoint)

inversePointsConvert :: (Num a, Floating a) => a -> a -> a -> a
inversePointsConvert lift gain pix = (gain - lift) * pix + lift

grade :: (Num a, Floating a) => a -> a -> a -> a -> a -> a -> a -> a -> a
grade blackpoint whitepoint lift gain multiply' offset' gamma =
    U.gamma gamma . offset offset' . multiply multiply' . inversePointsConvert lift gain . pointsConvert blackpoint whitepoint

hueCorrect :: BSpline.BSpline Float -> BSpline.BSpline Float ->
              BSpline.BSpline Float -> BSpline.BSpline Float -> BSpline.BSpline Float ->
              BSpline.BSpline Float -> BSpline.BSpline Float -> BSpline.BSpline Float ->
              A.Exp (RGB Float) -> A.Exp (RGB Float)
hueCorrect lum sat r g b rSup gSup bSup rgb = A.lift $ RGB r' g' b'
  where
    RGB pr pg pb = A.unlift rgb :: RGB (A.Exp Float)
    minOfRGB = (pr A.<* pg) A.? ((pb A.<* pr) A.? (pb,pr), (pb A.<* pg) A.? (pb,pg))
    HSV hue _ _ = toHSV (RGB pr pg pb)

    -- to compare the result of our hueCorrect and the Nuke's one just uncomment the below line
    -- hue = (6 * h A.>=* 5.0) A.? (6 * h - 5.0, 6 * h + 1.0)

    r' = ((process r hue) . (process lum hue) . (processSup rSup hue minOfRGB)) pr
    g' = ((process g hue) . (process lum hue) . (processSup gSup hue minOfRGB)) pg
    b' = ((process b hue) . (process lum hue) . (processSup bSup hue minOfRGB)) pb

    process :: BSpline.BSpline Float -> A.Exp Float -> A.Exp Float -> A.Exp Float
    process spline hue v = v * (BSpline.valueAt (A.use spline) hue)

    processSup :: BSpline.BSpline Float -> A.Exp Float -> A.Exp Float -> A.Exp Float -> A.Exp Float
    processSup spline hue w v = w + (v - w)*(BSpline.valueAt (A.use spline) hue)


colorCorrect :: forall a. (Elt a, IsFloating a)
             => Exp a       -- ^ contrast
             -> Exp a       -- ^ gamma
             -> Exp a       -- ^ gain
             -> Exp a       -- ^ offset
             -> Exp a       -- ^ input value
             -> Exp a
colorCorrect contrast' gamma' gain' offset' = offset offset' . gain'' gain' . U.gamma gamma' . contrast contrast'
              where gain'' b x = b * x -- U.gain is broken, tested with Nuke that it's simply multiplication

-- NOTE[mm]: pretty sure Nuke uses HSL colorspace for saturation manipulation. There are slight differences still,
--           but operating on HSV looks unalike to Nuke.
saturate :: (Elt t, IsFloating t, ColorConvert a HSL, ColorConvert HSL a) => Exp t -> a (Exp t) -> a (Exp t)
saturate saturation pix = toHSL pix & (\(HSL h s l) -> HSL h (s * saturation) l) & convertColor

saturateRGB :: Exp Float -> Exp Float -> Exp Float -> M.Matrix2 (RGB Float) -> (M.Matrix2 Float,M.Matrix2 Float,M.Matrix2 Float)
saturateRGB rs gs bs rgb = (rSaturated, gSaturated, bSaturated) where

    rgbRsaturated = M.map (A.lift1 ((saturate :: Exp Float -> RGB (Exp Float) -> RGB (Exp Float)) rs)) rgb
    rgbGsaturated = M.map (A.lift1 ((saturate :: Exp Float -> RGB (Exp Float) -> RGB (Exp Float)) gs)) rgb
    rgbBsaturated = M.map (A.lift1 ((saturate :: Exp Float -> RGB (Exp Float) -> RGB (Exp Float)) bs)) rgb

    --saturateOnHSV :: Exp Float -> RGB (Exp Float) -> RGB (Exp Float)
    --saturateOnHSV sat pix = toHSL pix & (\(HSL h s l) -> HSL h (s * sat) l) & toRGB

    rSaturated = M.map (\(A.unlift -> RGB r _ _) -> r) rgbRsaturated
    gSaturated = M.map (\(A.unlift -> RGB _ g _) -> g) rgbGsaturated
    bSaturated = M.map (\(A.unlift -> RGB _ _ b) -> b) rgbBsaturated


hsvTool :: forall a t. (Elt t, IsFloating t, ColorConvert a HSV, ColorConvert HSV a,
                        A.Lift Exp (a (Exp t)), A.Unlift Exp (a (Exp t)), Elt (A.Plain (a (Exp t))))
        => Exp (U.Range t) -> Exp t -> Exp t
        -> Exp (U.Range t) -> Exp t -> Exp t
        -> Exp (U.Range t) -> Exp t -> Exp t
        -> a (Exp t)
        -> a (Exp t)
hsvTool (A.unlift . variable -> hueRange) (variable -> hueRotation) (variable -> hueRolloff)
        (A.unlift . variable -> saturationRange) (variable -> saturationAdjustment) (variable -> saturationRolloff)
        (A.unlift . variable -> brightnessRange) (variable -> brightnessAdjustment) (variable -> brightnessRolloff) pix =
    A.unlift (conditionsFulfilled A.? (
        A.lift (HSV (rotation (hueRotation * cyclicPower hueRange hueRolloff h) h)
                    (s + saturationAdjustment * power saturationRange saturationRolloff s)
                    (v + brightnessAdjustment * power brightnessRange brightnessRolloff v)
                & convertColor :: a (Exp t))
        ,
        A.lift pix))
    where HSV h s v = toHSV pix
          rotation r hue = U.frac $ hue + r

          conditionsFulfilled = cyclicPower hueRange        hueRolloff        h A.>* 0
                                A.&&*
                                power       saturationRange saturationRolloff s A.>* 0
                                A.&&*
                                power       brightnessRange brightnessRolloff v A.>* 0

power :: forall a. (Elt a, IsFloating a) => U.Range (Exp a) -> Exp a -> Exp a -> Exp a
power range@(U.Range a' b') rolloff x =
    let a = variable a'
        b = variable b'

        rLeft  = U.Range (a - rolloff) a
        rRight = U.Range b (b + rolloff)

        rLeftEquation val = aLinear * val + bLinear
            where (aLinear, bLinear) = rise a rolloff

        rRightEquation val = aLinear * val + bLinear
            where (aLinear, bLinear) = fall b rolloff
    in A.cond (x `inRange` range) 1
       $ A.cond (x `inRange` rLeft) (rLeftEquation x)
       $ A.cond (x `inRange` rRight) (rRightEquation x)
       0

inRange :: (Elt a, IsScalar a) => Exp a -> U.Range (Exp a) -> Exp Bool
inRange val (U.Range a b) = val A.>=* a A.&&* val A.<=* b


-- | Used to compute power of hue rotation effect. It properly cycles on multiplies
--   of 360 degrees in both directions.
cyclicPower :: forall a. (Elt a, IsFloating a) => U.Range (Exp a) -> Exp a -> Exp a -> Exp a
cyclicPower (U.Range a' b') rolloff x =
    let a = variable a'
        b = variable b'
        (correct, pp') = intersection a b rolloff :: (Exp Bool, Exp a)
        pp = variable pp'
        (rLeft, rRight) = A.unlift (correct A.? (A.lift ((pp-1,a), (b,pp))
                                     , A.lift ((a-rolloff, a), (b, b+rolloff)))) :: (A.Exp (a, a), A.Exp (a, a))
        rLeftEquation val = A.cond (frL A./=* 0) (fxL val / frL) 1
        rRightEquation val = A.cond (frR A./=* 0) (1 - fxR val / frR) 1

        fxL val = U.frac $ val - tL
        frL     = U.frac $ a - tL

        fxR val = U.frac $ val - tR
        frR     = U.frac rolloff -- == b+r - tR == b+r - b == r

        tL      = U.frac $ a - rolloff
        tR      = U.frac b
    in check x (a,b) A.? (1,
       check x (A.unlift rLeft) A.? (rLeftEquation x,
       check x (A.unlift rRight) A.? (rRightEquation x, 0)))

intersection :: (A.Elt a, A.IsFloating a) => Exp a -> Exp a -> Exp a -> (Exp Bool, Exp a)
intersection a b r = (y A.>* 0 A.&&* y A.<* 1, x)
    where x = (b2 - b1) / (a1 - a2)
          y = a1*((b2-b1)/(a1-a2))+b1
          (a1, b1) = rise a r
          (a2, b2) = fall b r

rise :: Fractional t => t -> t -> (t, t)
rise a r = (1/r, 1-(a+1)/r)

fall :: Fractional t => t -> t -> (t, t)
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

-- | Lowers number of colors in a picture to 2^colors
posterize :: (Elt a, IsFloating a) => Exp a -> Exp a -> Exp a
posterize colors val = A.cond (colors A.==* 0) 0
                     $ A.cond (colors A.==* 1) 1
                     $ s * A.fromIntegral (A.floor $ val / t :: Exp Int)
    where s = 1 / (colors - 1)
          t = 1 / colors


type Vec3 a   = (Exp  a, Exp  a, Exp  a)
type Mat3x3 a = (Vec3 a, Vec3 a, Vec3 a)

type Vec4 a   = (Exp  a, Exp  a, Exp  a, Exp  a)
type Mat4x4 a = (Vec4 a, Vec4 a, Vec4 a, Vec4 a)

-- | Dispatches colorspace to required color matrix

type family ColorMatrix (colorspace :: * -> *) t :: *

type instance ColorMatrix CMY      t = Mat3x3 t
type instance ColorMatrix CMYK     t = Mat4x4 t
type instance ColorMatrix HSL      t = Mat3x3 t
type instance ColorMatrix HSV      t = Mat3x3 t
type instance ColorMatrix RGB      t = Mat3x3 t
type instance ColorMatrix RGBA     t = Mat4x4 t
type instance ColorMatrix YCbCr    t = Mat3x3 t
type instance ColorMatrix YCbCr_HD t = Mat3x3 t

-- | Multiplies given matrix by colour.
--   Matrix needs to be in a row major order and n x n size, where n is a number
--   of components in a colour.
--
--   Example:
--   ((1, 0, 0),     (0.5,     (0.5,
--    (0, 1, 1),  *   0.6,  =   1.6,
--    (0, 0, 1))      1.0)      1.0)

colorMatrix :: (Elt t, IsFloating t, MatrixMultiplication a) => ColorMatrix a t -> a (Exp t) -> a (Exp t)
colorMatrix = mmult

class MatrixMultiplication colorspace where
    mmult :: (Elt t, IsNum t) => ColorMatrix colorspace t -> colorspace (Exp t) -> colorspace (Exp t)

instance MatrixMultiplication CMY where
    mmult = mul3x3

instance MatrixMultiplication CMYK where
    mmult = mul4x4

instance MatrixMultiplication HSL where
    mmult = mul3x3

instance MatrixMultiplication HSV where
    mmult = mul3x3

instance MatrixMultiplication RGB where
    mmult = mul3x3

instance MatrixMultiplication RGBA where
    mmult = mul4x4

instance MatrixMultiplication YCbCr where
    mmult = mul3x3

instance MatrixMultiplication YCbCr_HD where
    mmult = mul3x3

mul3x3 :: (IsTuple (a (Exp t)), Elt t, IsNum t, TupleRepr (a (Exp t)) ~ TupleRepr (Vec3 t)) => Mat3x3 t -> a (Exp t) -> a (Exp t)
mul3x3 ((a, b, c), (d, e, f), (g, h, i)) pix = toTuple ((((), x'), y'), z')
    where ((((), x), y), z) = fromTuple pix
          x' = a * x + b * y + c * z
          y' = d * x + e * y + f * z
          z' = g * x + h * y + i * z

mul4x4 :: (IsTuple (a (Exp t)), Elt t, IsNum t, TupleRepr (a (Exp t)) ~ TupleRepr (Vec4 t))
       => Mat4x4 t -> a (Exp t) -> a (Exp t)
mul4x4 ((a, b, c, d), (e, f, g, h), (i, j, k, l), (m, n, o, p)) pix = toTuple (((((), x'), y'), z'), w')
    where (((((), x), y), z), w) = fromTuple pix
          x' = a * x + b * y + c * z + d * w
          y' = e * x + f * y + g * z + h * w
          z' = i * x + j * y + k * z + l * w
          w' = m * x + n * y + o * z + p * w


crosstalk :: BSpline.BSpline Float -- ^ red channel curve
          -> BSpline.BSpline Float -- ^ green channel curve
          -> BSpline.BSpline Float -- ^ blue channel curve
          -> BSpline.BSpline Float -- ^ r->g curve
          -> BSpline.BSpline Float -- ^ r->b curve
          -> BSpline.BSpline Float -- ^ g->r curve
          -> BSpline.BSpline Float -- ^ g->b curve
          -> BSpline.BSpline Float -- ^ b->r curve
          -> BSpline.BSpline Float -- ^ b->g curve
          -> A.Exp (RGB Float)
          -> A.Exp (RGB Float)
crosstalk redBezier greenBezier blueBezier
          redGreenBezier redBlueBezier
          greenRedBezier greenBlueBezier
          blueRedBezier blueGreenBezier
          rgb = A.lift $ RGB newRed newGreen newBlue
    where RGB r g b = A.unlift rgb :: RGB (A.Exp Float)

          newRed   = redBeziered   + greenToRed + blueToRed
          newGreen = greenBeziered + redToGreen + blueToGreen
          newBlue  = blueBeziered  + redToBlue  + greenToBlue

          redBeziered = BSpline.valueAt (A.use redBezier) r
          redToGreen  = BSpline.valueAt (A.use redGreenBezier) r
          redToBlue   = BSpline.valueAt (A.use redBlueBezier) r

          greenBeziered = BSpline.valueAt (A.use greenBezier) g
          greenToRed    = BSpline.valueAt (A.use greenRedBezier) g
          greenToBlue   = BSpline.valueAt (A.use greenBlueBezier) g

          blueBeziered = BSpline.valueAt (A.use blueBezier) b
          blueToRed    = BSpline.valueAt (A.use blueRedBezier) b
          blueToGreen  = BSpline.valueAt (A.use blueGreenBezier) b

