---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# LANGUAGE TemplateHaskell       #-}

module Flowbox.Graphics.Utils where

import Data.Array.Accelerate as A
import Data.Typeable

import Flowbox.Prelude as P
import Flowbox.Graphics.Utils.Accelerate



data Size a = Size {sizeW :: a, sizeH :: a}

data Range a = Range {rangeLo :: a, rangeHi :: a}
             deriving (Show, Typeable)
deriveAccelerate ''Range

range :: Float -> Float -> Exp (Range Float)
range lo hi = A.lift $ Range (variable lo) (variable hi)

invert :: Num a => a -> a
invert x = 1 - x

negate :: Num a => a -> a
negate x = -x

sign :: Num a => a -> a
sign x = (2 * x) - 1

parametrize :: Num a => a -> a -> a -> a
parametrize lo hi x = lo + x * (hi - lo)

bias :: (Elt t, IsFloating t) => Exp t -> Exp t -> Exp t
bias b x = (b A.>* 0) A.? (x ** logBase 0.5 b, 0)

gain :: (Elt t, IsFloating t) => Exp t -> Exp t -> Exp t
gain g x = 0.5 * (x A.<* 0.5 A.? (bias (2 * x) (1 - g) , 2 - bias (2 - 2 * x) (1 - g)))

gamma :: Floating a => a -> a -> a
gamma g x = x ** (1 / g)

compress :: Num a => a -> a -> a -> a
compress lo hi x = (hi - lo) * x + lo

expand :: (Elt t, IsFloating t) => Exp t -> Exp t -> Exp t -> Exp t
expand lo hi x = lo ==* hi A.? (x A.<* lo A.? (0 , 1) , (x - lo) / (hi - lo)) -- WATCH OUT! comparing Floating numbers!

remap :: Fractional a => a -> a -> a -> a -> a -> a
remap loA hiA loB hiB x = (x * (hiB-loB) - loA*hiB + hiA*loB) / (hiA-loA)

clamp :: (Elt a, IsScalar a) => Range (Exp a) -> Maybe (Range (Exp a)) -> Exp a -> Exp a
clamp (Range thresholdLo thresholdHi) clampTo v = (v A.<* thresholdLo A.?) $ case clampTo of
    Nothing                      -> (thresholdLo, v A.>* thresholdHi A.? (thresholdHi, v))
    Just (Range clampLo clampHi) -> (clampLo,     v A.>* thresholdHi A.? (clampHi,     v))

clamp' :: (Elt t, IsScalar t) => Exp t -> Exp t -> Exp t -> Exp t
clamp' low high val = (val A.>* high) A.? (high, (val A.<* low) A.? (low, val))

mix :: Num a => a -> a -> a -> a
mix amount oldValue newValue = invert amount * oldValue + amount * newValue


nonIntRem :: (Elt e, IsFloating e) => Exp e -> Exp e -> Exp e
nonIntRem x y = x - y * A.fromIntegral (A.truncate (x / y) :: Exp Int)

nonIntDiv :: (Elt e, IsFloating e) => Exp e -> Exp e -> Exp e
nonIntDiv x y = A.fromIntegral (A.truncate (x / y) :: Exp Int)

frac :: (Elt a, IsFloating a) => Exp a -> Exp a
frac x = x - A.fromIntegral (A.floor x :: Exp Int)

-- = TUPLES :D

-- TODO: Try to make accelerate tuples instances of Field1,2,3,4... from lens
fstTrio :: forall a b c. (Elt a, Elt b, Elt c) => Exp (a, b, c) -> Exp a
fstTrio e = let (x, _:: Exp b, _:: Exp c) = A.unlift e in x

sndTrio :: forall a b c. (Elt a, Elt b, Elt c) => Exp (a, b, c) -> Exp b
sndTrio e = let (_:: Exp a, x, _:: Exp c) = A.unlift e in x

trdTrio :: forall a b c. (Elt a, Elt b, Elt c) => Exp (a, b, c) -> Exp c
trdTrio e = let (_:: Exp a, _:: Exp b, x) = A.unlift e in x

-- = MORE TUPLES :D

fstQuad :: forall a b c d. (Elt a, Elt b, Elt c, Elt d) => Exp (a, b, c, d) -> Exp a
fstQuad e = let (x, _:: Exp b, _:: Exp c, _:: Exp d) = A.unlift e in x

sndQuad :: forall a b c d. (Elt a, Elt b, Elt c, Elt d) => Exp (a, b, c, d) -> Exp b
sndQuad e = let (_:: Exp a, x, _:: Exp c, _:: Exp d) = A.unlift e in x

trdQuad :: forall a b c d. (Elt a, Elt b, Elt c, Elt d) => Exp (a, b, c, d) -> Exp c
trdQuad e = let (_:: Exp a, _:: Exp b, x, _:: Exp d) = A.unlift e in x

frthQuad :: forall a b c d. (Elt a, Elt b, Elt c, Elt d) => Exp (a, b, c, d) -> Exp d
frthQuad e = let (_:: Exp a, _:: Exp b, _:: Exp c, x) = A.unlift e in x

-- = Accelerate utils

variable :: (Lift Exp e, Elt (Plain e)) => e -> Exp (Plain e)
variable a = the $ unit $ A.lift a
