---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE FlexibleContexts #-}

module Main where

import Flowbox.Graphics.Color hiding (b)
import Flowbox.Graphics.Composition.Generators.Filter
import Flowbox.Graphics.Composition.Generators.Rasterizer
import Flowbox.Graphics.Composition.Generators.Sampler
import Flowbox.Graphics.Composition.Generators.Structures
import Flowbox.Graphics.Image.Color hiding (b)
import Flowbox.Graphics.Utils
import Flowbox.Math.Matrix as M
import Flowbox.Prelude hiding (transform)

import qualified Data.Array.Accelerate as A
import Test.QuickCheck (quickCheck, (==>))
import Utils



input = "lena.png"
output = "out.png"

ex black ev = testFunction (exposure black ev) input output

--bp black = testFunction (blackpointConvert black) input output

--invertible black lift = testFunction (inverseBlackpointConvert lift . blackpointConvert black) input output
--invertibleWhite white gain = testFunction (inverseWhitepointConvert gain . whitepointConvert white) input output

--saturation v = testColor (saturate v) input output

--correct s c g g' o = testColor (colorCorrect s c g g' o) input output

--idHSL = testColor (toRGB . toHSL)

hsvT = testColor (hsvTool (Range (358/360) (359/360)) (180 / 360) (90/360) (Range 0 1) 0 0 (Range 0 1) (0) 0) input output

--inRange :: (IsScalar t, Elt t) => Exp t -> U.Range (Exp t) -> Exp Bool
inRange val (Range low high) = val >= low && val <= high

--rolloff :: (Elt a, IsFloating a) => U.Range (Exp a) -> Exp a -> Exp a -> Exp a
rolloff :: Range Float -> Float -> Float -> Float
rolloff range@(Range lo hi) roll val' = if' (val `Main.inRange` range)   1
                                        $ if' (roll == 0)          0
                                        $ if' (val <= (lo - roll)) 0
                                        $ if' (val >= (hi + roll)) 1
                                        $ if' (val <  lo)          ((1 / roll) * val + 1 - (lo / roll))
                                        $ (((-1) / roll) * val + 1 + (hi / roll)) -- covers val > hi
    where val = if val' > hi && hi + roll > 1 then (-1) * (val' - 1) else val'

--power x (a, b) r =
--    let (correct, pp) = intersection a b r :: (Exp Bool, Exp Float)
--        (rLeft, rRight) = A.unlift (correct A.? (A.lift ((pp-1,a), (b,pp))
--                                     , A.lift ((a-r, a), (b, b+r)))) :: (A.Exp (Float, Float), A.Exp (Float, Float))
--        rLeftEquation val = A.cond (frL A./=* 0) (fxL val / frL) 1
--        rRightEquation val = A.cond (frR A./=* 0) (1 - fxR val / frR) 1

--        fxL val = frac $ val - tL
--        frL = frac $ a - tL

--        fxR val = frac $ val - tR
--        frR = frac $ r -- == b+r - tR == b+r - b == r

--        tL = frac $ a - r
--        tR = frac $ b
--    in check x (a,b) A.? (1,
--       check x (A.unlift rLeft) A.? (rLeftEquation x,
--       check x (A.unlift rRight) A.? (rRightEquation x, 0)))

--power' x (a, b) r =
--    let (correct, pp) = intersection' a b r
--        (rLeft, rRight) = if correct then ((pp-1,a), (b,pp))
--                                     else ((a-r, a), (b, b+r))
--        --rLeftEquation val  = ((1 / r) * val + 1 - (a / r))
--        --rRightEquation val = (((-1) / r) * val + 1 + (b / r))
--        rLeftEquation val = if frL /= 0 then fxL val / frL else 1
--        rRightEquation val = if frR /= 0 then 1 - fxR val / frR else 1

--        fxL val = frac' $ val - tL
--        frL = frac' $ a - tL

--        fxR val = frac' $ val - tR
--        frR = frac' $ r -- == b+r - tR == b+r - b == r

--        tL = frac' $ a - r
--        tR = frac' $ b
--    in  if check' x (a,b)
--            then 1
--            else if check' x rLeft
--                     then rLeftEquation x
--                     else if check' x rRight
--                              then rRightEquation x
--                              else 0


----intersection :: ((Float, Float), (Float, Float)) -> ((Float, Float), (Float, Float)) -> Maybe (Float, Float)
--intersection :: (A.Elt a, A.IsFloating a) => Exp a -> Exp a -> Exp a -> (Exp Bool, Exp a)
--intersection a b r = (y A.>* 0 A.&&* y A.<* 1, x)
--    where x = (b2 - b1) / (a1 - a2)
--          y = a1*((b2-b1)/(a1-a2))+b1
--          (a1, b1) = rise a r
--          (a2, b2) = fall b r

--intersection' a b r = (y > 0 && y < 1, x)
--    where x = (b2 - b1) / (a1 - a2)
--          y = a1*((b2-b1)/(a1-a2))+b1
--          (a1, b1) = rise a r
--          (a2, b2) = fall b r

--rise a r = (1/r, 1-(a+1)/r)

--fall b r = ((-1)/r, 1+(b/r))

--check :: (A.Elt a, A.IsFloating a) => Exp a -> (Exp a, Exp a) -> Exp Bool
--check (frac -> x) (a,b) =
--    let a' = frac a
--        b' = frac b
--    in (b-a A.>=* 1)
--       A.||*
--       (x A.>=* a' A.&&* x A.<=* b')
--       A.||*
--       (b' A.<* a' A.&&* ((x A.>=* a' A.&&* x A.>=* b')
--           A.||*
--           (x A.<=* a' A.&&* x A.<=* b')))

frac' x = x - fromIntegral (floor x)
check' (frac' -> x) (a, b) = 
    let a' = frac' a
        b' = frac' b
    in  (b-a >= 1) || (x >= a' && x <= b') || (b' < a' && ((x >= a' && x >= b') || (x <= a' && x <= b')))


if' True  a _ = a
if' False _ b = b

main = do
    --putStrLn "Testing invertibility"
    --quickCheck $ \(p :: Float) w b -> (abs (w - b) > 0.001 && w > b && p > 0.001) ==> abs (inversePointsConvert b w (pointsConvert b w p) - p) < 1.0e-10
    
    --testFunction (grade 0.08 0.85 (-0.11) 1.85 1.45 (-0.46) 0.62) input output

    ex (-0.32) 0.3