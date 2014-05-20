---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts    #-}

module Flowbox.Graphics.Utils where

import           Data.Array.Accelerate (Exp)
import qualified Data.Array.Accelerate as A

import           Flowbox.Prelude       as P



data Size a = Size {sizeW :: a, sizeH :: a}
data Range a = Range {rangeLo :: a, rangeHi :: a}


-- THINK[2]: about invert and invert' and their names
invert :: Num a => a -> a
invert x = 1 - x

invert' :: Num a => a -> a
invert' x = -x

sign :: Num a => a -> a
sign x = (2 * x) - 1

parametrize :: Num a => a -> a -> a -> a
parametrize lo hi x = lo + x * (hi - lo)

bias :: (A.Elt t, A.Elt t1, A.IsNum t, A.IsIntegral t1, A.IsFloating t1) => Exp t1 -> Exp t -> Exp t
bias b x = (b A.>* 0) A.? (x ^ logBase 0.5 b, 0)

gain :: (A.Elt t, A.Elt t1, A.IsIntegral t1, A.IsFloating t, A.IsFloating t1) => Exp t -> Exp t1 -> Exp t
gain g x = 0.5 * (x A.<* 0.5 A.? (bias (2 * x) (1 - g) , 2 - bias (2 - 2 * x) (1 - g)))

gamma :: (Fractional b, Integral b, Num a) => b -> a -> a
gamma g x = x ^ (1 / g)

compress :: Num a => a -> a -> a -> a
compress lo hi x = (hi - lo) * x + lo

expand :: (A.Elt t, A.IsFloating t) => Exp t -> Exp t -> Exp t -> Exp t
expand lo hi x = lo A.==* hi A.? (x A.<* lo A.? (0 , 1) , (x - lo) / (hi - lo)) -- WATCH OUT! comparing Floating numbers!

remap :: Fractional a => a -> a -> a -> a -> a -> a
remap loA hiA loB hiB x = (x * (hiB-loB) - loA*hiB + hiA*loB) / (hiA-loA)

clamp :: (A.Elt a, A.IsScalar a) => Range (Exp a) -> Maybe (Range (Exp a)) -> Exp a -> Exp a
clamp (Range thresholdLo thresholdHi) clampTo v = (v A.<* thresholdLo A.?) $ case clampTo of
    Nothing                      -> (thresholdLo, v A.>* thresholdHi A.? (thresholdHi, v))
    Just (Range clampLo clampHi) -> (clampLo,     v A.>* thresholdHi A.? (clampHi,     v))

mix :: (A.Elt a, A.IsNum a) => Exp a -> Exp a -> Exp a -> Exp a
mix amount oldValue newValue = invert amount * oldValue + amount * newValue


nonIntRem :: (A.Elt e, A.IsFloating e) => Exp e -> Exp e -> Exp e
nonIntRem x y = x - y * A.fromIntegral (A.truncate (x / y) :: Exp Int)

nonIntDiv :: (A.Elt e, A.IsFloating e) => Exp e -> Exp e -> Exp e
nonIntDiv x y = A.fromIntegral (A.truncate (x / y) :: Exp Int)


-- = TUPLES :D

-- TODO: Try to make accelerate tuples instances of Field1,2,3,4... from lens
fstTrio :: forall f a b c. A.Unlift f (f a, f b, f c) => f (A.Plain (f a), A.Plain (f b), A.Plain (f c)) -> f a
fstTrio e = let (x, _:: f b, _:: f c) = A.unlift e in x

sndTrio :: forall f a b c. A.Unlift f (f a, f b, f c) => f (A.Plain (f a), A.Plain (f b), A.Plain (f c)) -> f b
sndTrio e = let (_:: f a, x, _:: f c) = A.unlift e in x

trdTrio :: forall f a b c. A.Unlift f (f a, f b, f c) => f (A.Plain (f a), A.Plain (f b), A.Plain (f c)) -> f c
trdTrio e = let (_:: f a, _:: f b, x) = A.unlift e in x

-- = MORE TUPLES :D

fstQuad :: forall f a b c d. A.Unlift f (f a, f b, f c, f d) => f (A.Plain (f a), A.Plain (f b), A.Plain (f c), A.Plain (f d)) -> f a
fstQuad e = let (x, _:: f b, _:: f c, _:: f d) = A.unlift e in x

sndQuad :: forall f a b c d. A.Unlift f (f a, f b, f c, f d) => f (A.Plain (f a), A.Plain (f b), A.Plain (f c), A.Plain (f d)) -> f b
sndQuad e = let (_:: f a, x, _:: f c, _:: f d) = A.unlift e in x

trdQuad :: forall f a b c d. A.Unlift f (f a, f b, f c, f d) => f (A.Plain (f a), A.Plain (f b), A.Plain (f c), A.Plain (f d)) -> f c
trdQuad e = let (_:: f a, _:: f b, x, _:: f d) = A.unlift e in x

frthQuad :: forall f a b c d. A.Unlift f (f a, f b, f c, f d) => f (A.Plain (f a), A.Plain (f b), A.Plain (f c), A.Plain (f d)) -> f d
frthQuad e = let (_:: f a, _:: f b, _:: f c, x) = A.unlift e in x
