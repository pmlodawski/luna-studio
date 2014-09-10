---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE RankNTypes             #-}
{-# LANGUAGE RecordWildCards        #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE TypeSynonymInstances   #-}
{-# LANGUAGE ViewPatterns           #-}
{-# LANGUAGE FunctionalDependencies #-}

module Flowbox.Math.Matrix (
    module Flowbox.Math.Matrix,
    (A.:.)(..),
    A.Boundary,
    A.DIM0,
    A.DIM1,
    A.DIM2,
    A.DIM3,
    A.DIM4,
    A.DIM5,
    A.DIM6,
    A.DIM7,
    A.DIM8,
    A.DIM9,
    A.Elt,
    A.Exp,
    A.FullShape,
    A.IsFloating,
    A.IsIntegral,
    A.IsNum,
    A.IsScalar,
    A.Segments,
    A.Shape,
    A.Slice,
    A.SliceShape,
    A.Stencil,
    A.Z(..)
) where

import qualified Data.Array.Accelerate                 as A
import qualified Data.Array.Accelerate.Array.Sugar     as Sugar
import qualified Data.Array.Accelerate.Math.FFT        as A
import qualified Data.Array.Accelerate.Math.Complex    as A
import qualified Data.Array.Accelerate.Math.DFT.Centre as A
import qualified Data.Array.Accelerate.IO              as A

import qualified Math.Coordinate.Cartesian as Cartesian

import Data.Complex                        (mkPolar)
import Data.Vector.Storable.Mutable hiding (set)
import Foreign.Ptr

import Flowbox.Prelude as P hiding (use, (<*), (?), (++), map, zipWith, set)



data Matrix ix a = Raw (A.Array ix a)
                 | Delayed (A.Acc (A.Array ix a))
                 deriving (Show)

type Scalar  a = Matrix A.DIM0 a
type Vector  a = Matrix A.DIM1 a
type Matrix2 a = Matrix A.DIM2 a

type Backend = forall a . A.Arrays a => A.Acc a -> a

-- == Instances ==
instance (A.Elt e) => Monoid (Vector e) where
    mempty = fromList (A.Z A.:. 0) []
    mappend a b = a ++ b

-- == Helpers ==

type EDIM1 = A.DIM0 A.:. A.Exp Int
type EDIM2 = EDIM1 A.:. A.Exp Int
type EDIM3 = EDIM2 A.:. A.Exp Int
type EDIM4 = EDIM3 A.:. A.Exp Int

accMatrix :: (A.Elt a, A.Shape ix) => Matrix ix a -> A.Acc (A.Array ix a)
accMatrix mat = case mat of
    Raw     m -> A.use m
    Delayed m -> m

compute :: (A.Elt a, A.Shape ix) => Backend -> Matrix ix a -> Matrix ix a
compute backend mat = Raw $ compute' backend mat

compute' :: (A.Elt a, A.Shape ix) => Backend -> Matrix ix a -> A.Array ix a
compute' backend mat = case mat of
    Raw     m -> m
    Delayed m -> backend m

-- == A.Accessors ==

-- = Scalar reduction =
sfoldl :: (A.Slice sh, A.Shape sh, A.Elt a, A.Elt b)
       => (A.Exp a -> A.Exp b -> A.Exp a)
       -> A.Exp a -> A.Exp sh
       -> Matrix (sh A.:. Int) b -> A.Exp a
sfoldl f z ix mat = A.sfoldl f z ix (accMatrix mat)

-- = Indexing =

(!) :: (A.Shape ix, A.Elt e) => Matrix ix e -> A.Exp ix -> A.Exp e
(!) mat = (A.!) $ accMatrix mat

(!!) :: (A.Shape ix, A.Elt e) => Matrix ix e -> A.Exp Int -> A.Exp e
(!!) mat = (A.!!) $ accMatrix mat

the :: A.Elt e => Scalar e -> A.Exp e
the mat = A.the $ accMatrix mat

-- = A.Shape Information =

empty :: (A.Shape ix, A.Elt e) => Matrix ix e -> A.Exp Bool
empty mat = A.null $ accMatrix mat

shape :: (A.Shape ix, A.Elt e) => Matrix ix e -> A.Exp ix
shape mat = A.shape $ accMatrix mat

size :: (A.Shape ix, A.Elt e) => Matrix ix e -> A.Exp Int
size mat = A.size $ accMatrix mat

shapeSize :: A.Shape ix => A.Exp ix -> A.Exp Int
shapeSize = A.shapeSize

-- = Extracting sub-arrays =

slice :: (A.Slice slix, A.Elt e) => Matrix (A.FullShape slix) e -> A.Exp slix -> Matrix (A.SliceShape slix) e
slice mat fragments = Delayed $ A.slice (accMatrix mat) fragments

take :: (A.Elt e) => A.Exp Int -> Vector e -> Vector e
take elems mat = Delayed $ A.take elems (accMatrix mat)

-- == Construction ==

-- = Introduction =

use :: (A.Elt a, A.Shape ix) => Matrix ix a -> Matrix ix a
use mat = Delayed $ accMatrix mat

unit :: A.Elt e => A.Exp e -> Scalar e
unit e = Delayed $ A.unit e


-- = Initialisation =

generate :: (A.Shape ix, A.Elt e) => A.Exp ix -> (A.Exp ix -> A.Exp e) -> Matrix ix e
generate sh f = Delayed $ A.generate sh f

replicate :: (A.Slice slix, A.Elt e)
    => A.Exp slix -> Matrix (A.SliceShape slix) e -> Matrix (A.FullShape slix) e
replicate sh m = Delayed $ A.replicate sh $ accMatrix m

fill :: (A.Shape ix, A.Elt e) => A.Exp ix -> A.Exp e -> Matrix ix e
fill sh x = Delayed $ A.fill sh x


-- = Enumeration =

enumFromN :: (A.Shape ix, A.Elt e, A.IsNum e) => A.Exp ix -> A.Exp e -> Matrix ix e
enumFromN sh n = Delayed $ A.enumFromN sh n

enumFromStepN :: (A.Shape ix, A.Elt e, A.IsNum e)
    => A.Exp ix -> A.Exp e -> A.Exp e -> Matrix ix e
enumFromStepN sh n s = Delayed $ A.enumFromStepN sh n s

-- = Pipelining =
infixl 1 >->
(>->) :: (A.Shape ixa, A.Shape ixb, A.Shape ixc, A.Elt a, A.Elt b, A.Elt c)
      => (Matrix ixa a -> Matrix ixb b) -> (Matrix ixb b -> Matrix ixc c) -> Matrix ixa a -> Matrix ixc c
(>->) a b mat = Delayed $ (ftrans a A.>-> ftrans b) (accMatrix mat)

ftrans :: (A.Shape ixa, A.Shape ixb, A.Elt a, A.Elt b) => (Matrix ixa a -> Matrix ixb b) -> A.Acc (A.Array ixa a) -> A.Acc (A.Array ixb b)
ftrans f a = accMatrix $ f (Delayed a)

-- = Concatenation =

(++) :: (A.Slice ix, A.Shape ix, A.Elt e) => Matrix (ix A.:. Int) e -> Matrix (ix A.:. Int) e -> Matrix (ix A.:. Int) e
(++) chan1 chan2 = Delayed $ accMatrix chan1 A.++ accMatrix chan2


-- == Modifying arrays ==

-- = A.Shape manipulation =

reshape :: (A.Shape ix, A.Shape ix', A.Elt e) => A.Exp ix -> Matrix ix' e -> Matrix ix e
reshape sh mat = Delayed $ A.reshape sh (accMatrix mat)

flatten :: (A.Elt a, A.Shape ix) => Matrix ix a -> Vector a
flatten mat = Delayed $ A.flatten (accMatrix mat)

-- = Permutations =

transpose :: A.Elt e => Matrix2 e -> Matrix2 e
transpose mat = Delayed $ A.transpose $ accMatrix mat

permute :: (A.Shape ix, A.Shape ix', A.Elt a)
        => (A.Exp a -> A.Exp a -> A.Exp a)
        -> Matrix ix' a
        -> (A.Exp ix -> A.Exp ix')
        -> Matrix ix a
        -> Matrix ix' a
permute combination defaults permutation array =
    Delayed $ A.permute combination (accMatrix defaults) permutation (accMatrix array)

-- == Element-wise operations ==

-- = Mapping =

map :: (A.Shape ix, A.Elt a, A.Elt b)
    => (A.Exp a -> A.Exp b) -> Matrix ix a -> Matrix ix b
map f mat = Delayed $ A.map f (accMatrix mat)

-- = Zipping =

zipWith :: (A.Shape ix, A.Elt a, A.Elt b, A.Elt c)
    => (A.Exp a -> A.Exp b -> A.Exp c)
    -> Matrix ix a
    -> Matrix ix b
    -> Matrix ix c
zipWith f mat1 mat2 = Delayed $ A.zipWith f (accMatrix mat1) (accMatrix mat2)

zipWith3 :: (A.Shape ix, A.Elt a, A.Elt b, A.Elt c, A.Elt d)
    => (A.Exp a -> A.Exp b -> A.Exp c -> A.Exp d)
    -> Matrix ix a
    -> Matrix ix b
    -> Matrix ix c
    -> Matrix ix d
zipWith3 f mat1 mat2 mat3 = Delayed $ A.zipWith3 f (accMatrix mat1) (accMatrix mat2) (accMatrix mat3)

zipWith4 :: (A.Shape ix, A.Elt a, A.Elt b, A.Elt c, A.Elt d, A.Elt e)
    => (A.Exp a -> A.Exp b -> A.Exp c -> A.Exp d -> A.Exp e)
    -> Matrix ix a
    -> Matrix ix b
    -> Matrix ix c
    -> Matrix ix d
    -> Matrix ix e
zipWith4 f mat1 mat2 mat3 mat4 = Delayed $ A.zipWith4 f (accMatrix mat1) (accMatrix mat2) (accMatrix mat3) (accMatrix mat4)

zipWith5 :: (A.Shape ix, A.Elt a, A.Elt b, A.Elt c, A.Elt d, A.Elt e, A.Elt f)
    => (A.Exp a -> A.Exp b -> A.Exp c -> A.Exp d -> A.Exp e -> A.Exp f)
    -> Matrix ix a
    -> Matrix ix b
    -> Matrix ix c
    -> Matrix ix d
    -> Matrix ix e
    -> Matrix ix f
zipWith5 f mat1 mat2 mat3 mat4 mat5 = Delayed $ A.zipWith5 f (accMatrix mat1) (accMatrix mat2) (accMatrix mat3) (accMatrix mat4) (accMatrix mat5)

zipWith6 :: (A.Shape ix, A.Elt a, A.Elt b, A.Elt c, A.Elt d, A.Elt e, A.Elt f, A.Elt g)
    => (A.Exp a -> A.Exp b -> A.Exp c -> A.Exp d -> A.Exp e -> A.Exp f -> A.Exp g)
    -> Matrix ix a
    -> Matrix ix b
    -> Matrix ix c
    -> Matrix ix d
    -> Matrix ix e
    -> Matrix ix f
    -> Matrix ix g
zipWith6 f mat1 mat2 mat3 mat4 mat5 mat6 = Delayed $ A.zipWith6 f (accMatrix mat1) (accMatrix mat2) (accMatrix mat3) (accMatrix mat4) (accMatrix mat5) (accMatrix mat6)

zipWith7 :: (A.Shape ix, A.Elt a, A.Elt b, A.Elt c, A.Elt d, A.Elt e, A.Elt f, A.Elt g, A.Elt h)
    => (A.Exp a -> A.Exp b -> A.Exp c -> A.Exp d -> A.Exp e -> A.Exp f -> A.Exp g -> A.Exp h)
    -> Matrix ix a
    -> Matrix ix b
    -> Matrix ix c
    -> Matrix ix d
    -> Matrix ix e
    -> Matrix ix f
    -> Matrix ix g
    -> Matrix ix h
zipWith7 f mat1 mat2 mat3 mat4 mat5 mat6 mat7 = Delayed $ A.zipWith7 f (accMatrix mat1) (accMatrix mat2) (accMatrix mat3) (accMatrix mat4) (accMatrix mat5) (accMatrix mat6) (accMatrix mat7)

zipWith8 :: (A.Shape ix, A.Elt a, A.Elt b, A.Elt c, A.Elt d, A.Elt e, A.Elt f, A.Elt g, A.Elt h, A.Elt i)
    => (A.Exp a -> A.Exp b -> A.Exp c -> A.Exp d -> A.Exp e -> A.Exp f -> A.Exp g -> A.Exp h -> A.Exp i)
    -> Matrix ix a
    -> Matrix ix b
    -> Matrix ix c
    -> Matrix ix d
    -> Matrix ix e
    -> Matrix ix f
    -> Matrix ix g
    -> Matrix ix h
    -> Matrix ix i
zipWith8 f mat1 mat2 mat3 mat4 mat5 mat6 mat7 mat8 = Delayed $ A.zipWith8 f (accMatrix mat1) (accMatrix mat2) (accMatrix mat3) (accMatrix mat4) (accMatrix mat5) (accMatrix mat6) (accMatrix mat7) (accMatrix mat8)

zipWith9 :: (A.Shape ix, A.Elt a, A.Elt b, A.Elt c, A.Elt d, A.Elt e, A.Elt f, A.Elt g, A.Elt h, A.Elt i, A.Elt j)
    => (A.Exp a -> A.Exp b -> A.Exp c -> A.Exp d -> A.Exp e -> A.Exp f -> A.Exp g -> A.Exp h -> A.Exp i -> A.Exp j)
    -> Matrix ix a
    -> Matrix ix b
    -> Matrix ix c
    -> Matrix ix d
    -> Matrix ix e
    -> Matrix ix f
    -> Matrix ix g
    -> Matrix ix h
    -> Matrix ix i
    -> Matrix ix j
zipWith9 f mat1 mat2 mat3 mat4 mat5 mat6 mat7 mat8 mat9 = Delayed $ A.zipWith9 f (accMatrix mat1) (accMatrix mat2) (accMatrix mat3) (accMatrix mat4) (accMatrix mat5) (accMatrix mat6) (accMatrix mat7) (accMatrix mat8) (accMatrix mat9)


zip :: (A.Shape ix, A.Elt a, A.Elt b)
    => Matrix ix a
    -> Matrix ix b
    -> Matrix ix (a, b)
zip  mat1 mat2 = Delayed $ A.zip (accMatrix mat1) (accMatrix mat2)

zip3 :: (A.Shape ix, A.Elt a, A.Elt b, A.Elt c)
    => Matrix ix a
    -> Matrix ix b
    -> Matrix ix c
    -> Matrix ix (a, b, c)
zip3 mat1 mat2 mat3 = Delayed $ A.zip3 (accMatrix mat1) (accMatrix mat2) (accMatrix mat3)

zip4 :: (A.Shape ix, A.Elt a, A.Elt b, A.Elt c, A.Elt d)
    => Matrix ix a
    -> Matrix ix b
    -> Matrix ix c
    -> Matrix ix d
    -> Matrix ix (a, b, c, d)
zip4 mat1 mat2 mat3 mat4 = Delayed $ A.zip4 (accMatrix mat1) (accMatrix mat2) (accMatrix mat3) (accMatrix mat4)

zip5 :: (A.Shape ix, A.Elt a, A.Elt b, A.Elt c, A.Elt d, A.Elt e)
    => Matrix ix a
    -> Matrix ix b
    -> Matrix ix c
    -> Matrix ix d
    -> Matrix ix e
    -> Matrix ix (a, b, c, d, e)
zip5 mat1 mat2 mat3 mat4 mat5 = Delayed $ A.zip5 (accMatrix mat1) (accMatrix mat2) (accMatrix mat3) (accMatrix mat4) (accMatrix mat5)

zip6 :: (A.Shape ix, A.Elt a, A.Elt b, A.Elt c, A.Elt d, A.Elt e, A.Elt f)
    => Matrix ix a
    -> Matrix ix b
    -> Matrix ix c
    -> Matrix ix d
    -> Matrix ix e
    -> Matrix ix f
    -> Matrix ix (a, b, c, d, e, f)
zip6 mat1 mat2 mat3 mat4 mat5 mat6 = Delayed $ A.zip6 (accMatrix mat1) (accMatrix mat2) (accMatrix mat3) (accMatrix mat4) (accMatrix mat5) (accMatrix mat6)

zip7 :: (A.Shape ix, A.Elt a, A.Elt b, A.Elt c, A.Elt d, A.Elt e, A.Elt f, A.Elt g)
    => Matrix ix a
    -> Matrix ix b
    -> Matrix ix c
    -> Matrix ix d
    -> Matrix ix e
    -> Matrix ix f
    -> Matrix ix g
    -> Matrix ix (a, b, c, d, e, f, g)
zip7 mat1 mat2 mat3 mat4 mat5 mat6 mat7 = Delayed $ A.zip7 (accMatrix mat1) (accMatrix mat2) (accMatrix mat3) (accMatrix mat4) (accMatrix mat5) (accMatrix mat6) (accMatrix mat7)

zip8 :: (A.Shape ix, A.Elt a, A.Elt b, A.Elt c, A.Elt d, A.Elt e, A.Elt f, A.Elt g, A.Elt h)
    => Matrix ix a
    -> Matrix ix b
    -> Matrix ix c
    -> Matrix ix d
    -> Matrix ix e
    -> Matrix ix f
    -> Matrix ix g
    -> Matrix ix h
    -> Matrix ix (a, b, c, d, e, f, g, h)
zip8 mat1 mat2 mat3 mat4 mat5 mat6 mat7 mat8 = Delayed $ A.zip8 (accMatrix mat1) (accMatrix mat2) (accMatrix mat3) (accMatrix mat4) (accMatrix mat5) (accMatrix mat6) (accMatrix mat7) (accMatrix mat8)

zip9 :: (A.Shape ix, A.Elt a, A.Elt b, A.Elt c, A.Elt d, A.Elt e, A.Elt f, A.Elt g, A.Elt h, A.Elt i)
    => Matrix ix a
    -> Matrix ix b
    -> Matrix ix c
    -> Matrix ix d
    -> Matrix ix e
    -> Matrix ix f
    -> Matrix ix g
    -> Matrix ix h
    -> Matrix ix i
    -> Matrix ix (a, b, c, d, e, f, g, h, i)
zip9 mat1 mat2 mat3 mat4 mat5 mat6 mat7 mat8 mat9 = Delayed $ A.zip9 (accMatrix mat1) (accMatrix mat2) (accMatrix mat3) (accMatrix mat4) (accMatrix mat5) (accMatrix mat6) (accMatrix mat7) (accMatrix mat8) (accMatrix mat9)

-- = Unzipping =

unzip :: (A.Shape ix, A.Elt a)
    => Matrix ix (a, a)
    -> (Matrix ix a, Matrix ix a)
unzip mat = over each Delayed $ A.unzip (accMatrix mat)

unzip3 :: (A.Shape ix, A.Elt a)
    => Matrix ix (a, a, a)
    -> (Matrix ix a, Matrix ix a, Matrix ix a)
unzip3 mat = over each Delayed $ A.unzip3 (accMatrix mat)

unzip4 :: (A.Shape ix, A.Elt a)
    => Matrix ix (a, a, a, a)
    -> (Matrix ix a, Matrix ix a, Matrix ix a, Matrix ix a)
unzip4 mat = over each Delayed $ A.unzip4 (accMatrix mat)

unzip5 :: (A.Shape ix, A.Elt a)
    => Matrix ix (a, a, a, a, a)
    -> (Matrix ix a, Matrix ix a, Matrix ix a, Matrix ix a, Matrix ix a)
unzip5 mat = over each Delayed $ A.unzip5 (accMatrix mat)

unzip6 :: (A.Shape ix, A.Elt a)
    => Matrix ix (a, a, a, a, a, a)
    -> (Matrix ix a, Matrix ix a, Matrix ix a, Matrix ix a, Matrix ix a, Matrix ix a)
unzip6 mat = over each Delayed $ A.unzip6 (accMatrix mat)

unzip7 :: (A.Shape ix, A.Elt a)
    => Matrix ix (a, a, a, a, a, a, a)
    -> (Matrix ix a, Matrix ix a, Matrix ix a, Matrix ix a, Matrix ix a, Matrix ix a, Matrix ix a)
unzip7 mat = over each Delayed $ A.unzip7 (accMatrix mat)

unzip8 :: (A.Shape ix, A.Elt a)
    => Matrix ix (a, a, a, a, a, a, a, a)
    -> (Matrix ix a, Matrix ix a, Matrix ix a, Matrix ix a, Matrix ix a, Matrix ix a, Matrix ix a, Matrix ix a)
unzip8 mat = over each Delayed $ A.unzip8 (accMatrix mat)

unzip9 :: (A.Shape ix, A.Elt a)
    => Matrix ix (a, a, a, a, a, a, a, a, a)
    -> (Matrix ix a, Matrix ix a, Matrix ix a, Matrix ix a, Matrix ix a, Matrix ix a, Matrix ix a, Matrix ix a, Matrix ix a)
unzip9 mat = over each Delayed $ A.unzip9 (accMatrix mat)


-- == Folding ==

fold :: (A.Shape ix, A.Elt a) => (A.Exp a -> A.Exp a -> A.Exp a) -> A.Exp a -> Matrix (ix A.:. Int) a -> Matrix ix a
fold f acc mat = Delayed $ A.fold f acc $ accMatrix mat

fold1 :: (A.Shape ix, A.Elt a) => (A.Exp a -> A.Exp a -> A.Exp a) -> Matrix (ix A.:. Int) a -> Matrix ix a
fold1 f mat = Delayed $ A.fold1 f $ accMatrix mat

foldAll :: (A.Shape sh, A.Elt a) => (A.Exp a -> A.Exp a -> A.Exp a) -> A.Exp a -> Matrix sh a -> Scalar a
foldAll f acc mat = Delayed $ A.foldAll f acc $ accMatrix mat

fold1All :: (A.Shape sh, A.Elt a) => (A.Exp a -> A.Exp a -> A.Exp a) -> Matrix sh a -> Scalar a
fold1All f mat = Delayed $ A.fold1All f $ accMatrix mat

foldSeg :: (A.Shape ix, A.Elt a, A.Elt i, A.IsIntegral i) => (A.Exp a -> A.Exp a -> A.Exp a) -> A.Exp a -> Matrix (ix A.:. Int) a -> A.Acc (A.Segments i) -> Matrix (ix A.:. Int) a
foldSeg f acc mat segments = Delayed $ A.foldSeg f acc (accMatrix mat) segments

fold1Seg :: (A.Shape ix, A.Elt a, A.Elt i, A.IsIntegral i) => (A.Exp a -> A.Exp a -> A.Exp a) -> Matrix (ix A.:. Int) a -> A.Acc (A.Segments i) -> Matrix (ix A.:. Int) a
fold1Seg f mat segments = Delayed $ A.fold1Seg f (accMatrix mat) segments

-- == Specialised folds ==

all :: (A.Shape sh, A.Elt e) => (A.Exp e -> A.Exp Bool) -> Matrix sh e -> Scalar Bool
all pred mat = Delayed $ A.all pred (accMatrix mat)

any :: (A.Shape sh, A.Elt e) => (A.Exp e -> A.Exp Bool) -> Matrix sh e -> Scalar Bool
any pred mat = Delayed $ A.any pred (accMatrix mat)

sum :: (A.IsNum a, A.Shape sh, A.Elt a) => Matrix sh a -> Scalar a
sum mat = Delayed $ A.sum (accMatrix mat)

product :: (A.IsNum a, A.Shape sh, A.Elt a) => Matrix sh a -> Scalar a
product mat = Delayed $ A.product (accMatrix mat)

minimum :: (A.IsNum a, A.Shape sh, A.Elt a) => Matrix sh a -> Scalar a
minimum mat = Delayed $ A.minimum (accMatrix mat)

maximum :: (A.IsNum a, A.Shape sh, A.Elt a) => Matrix sh a -> Scalar a
maximum mat = Delayed $ A.maximum (accMatrix mat)

-- == A.Stencil ==

stencil :: (A.Shape ix, A.Elt a, A.Elt b, A.Stencil ix a stencil)
    => (stencil -> A.Exp b) -> A.Boundary a -> Matrix ix a -> Matrix ix b
stencil f b mat = Delayed $ A.stencil f b (accMatrix mat)

stencil2 :: (A.Shape ix, A.Elt a, A.Elt b, A.Elt c, A.Stencil ix a stencil1, A.Stencil ix b stencil2)
    => (stencil1 -> stencil2 -> A.Exp c) -> A.Boundary a -> Matrix ix a -> A.Boundary b -> Matrix ix b -> Matrix ix c
stencil2 f b1 mat1 b2 mat2 = Delayed $ A.stencil2 f b1 (accMatrix mat1) b2 (accMatrix mat2)


-- == Operations ==

-- = A.Shape manipulation =

--index3 :: (A.Elt i, A.Slice (A.Z A.A.:. i), A.Slice (A.Z A.A.:. i A.A.:. i))
--    => A.Exp i -> A.Exp i -> A.Exp i -> A.Exp (A.Z A.A.:. i A.A.:. i A.A.:. i)
--index3 i j k = A.lift (A.Z A.A.:. i A.A.:. j A.A.:. k)

--lift2Dto3D :: (A.Elt a) => Matrix A.DIM2 a -> Matrix A.DIM3 a
--lift2Dto3D channel = reshape (index3 a b 1) channel
--    where (A.Z A.A.:. a A.A.:. b) = A.unlift $ shape channel

-- == Conversions ==

fromList :: (A.Shape sh, A.Elt e) => sh -> [e] -> Matrix sh e
fromList sh mat = Raw $ A.fromList sh mat

toList :: (A.Shape sh, A.Elt e) => Backend -> Matrix sh e -> [e]
toList b mat = A.toList $ compute' b mat


-- == FFT ==

fft :: (A.Elt e, A.IsFloating e) => Backend -> Matrix2 e -> Matrix2 (A.Complex e)
fft backend mat' = Delayed $ A.fft2D' A.Forward width height arrCentered
    where cMat = compute' backend mat'
          A.Z A.:. height A.:. width = A.arrayShape cMat
          mat = A.use cMat
          arrComplex  = A.map (\r -> A.lift (r A.:+ A.constant 0)) mat
          arrCentered = A.centre2D arrComplex

inverseFFT :: (A.Elt e, A.IsFloating e) => Backend -> Matrix2 (A.Complex e) -> Matrix2 e
inverseFFT backend mat' = Delayed $ A.map A.magnitude $ A.fft2D' A.Inverse width height mat
    where cMat = compute' backend mat'
          A.Z A.:. height A.:. width = A.arrayShape cMat
          mat = A.use cMat

-- trans :: Frequency -> Amplitude -> FrequencyResponse
fftFilter :: (A.Elt e, A.IsFloating e) => Backend -> (A.Exp e -> A.Exp e -> A.Exp e) -> Matrix2 e -> Matrix2 e
fftFilter backend trans mat = inverseFFT backend fftProc
    where matFFT = fft backend mat
          mag = map A.magnitude matFFT
          pha = map A.phase matFFT

          polar :: (A.Elt e, A.IsFloating e) => A.Exp e -> A.Exp e -> A.Exp (A.Complex e)
          polar r theta = A.lift $ r * cos theta A.:+ r * sin theta

          magSh = shape mag
          tmag = generate magSh wrapper
          A.Z A.:. height A.:. width = A.unlift $ magSh :: EDIM2
          wrapper ix@(A.unlift -> A.Z A.:. y A.:. x :: EDIM2) = trans freq (mag ! ix)
              where iW = A.fromIntegral width / 2
                    iH = A.fromIntegral height / 2
                    xFreq = let x' = A.fromIntegral x - iW in A.cond (x' A.>* iW) (x' - iW) x'
                    yFreq = let y' = A.fromIntegral y - iH in A.cond (y' A.>* iH) (y' - iH) y'
                    freq = sqrt $ xFreq * xFreq + yFreq * yFreq

          fftProc = zipWith polar tmag pha

-- == Mutable, CPU based matrix processing

data MMatrix m a = MMatrix { vector :: m a
                           , canvas :: A.DIM2
                           }

type MImage = MMatrix IOVector

mutableProcess :: forall a . (A.Elt a, Storable a, A.BlockPtrs (Sugar.EltRepr a) ~ ((), Ptr a)) 
       => Backend 
       -> (MImage a -> IO ())
       -> Matrix2 a -> IO (Matrix2 a)
mutableProcess b action mat = do
    let gpuMat   = compute' b mat
    let gpuShape = A.arrayShape gpuMat
    cpuMat <- unsafeNew $ A.arraySize gpuShape  :: IO (IOVector a)
    newGpuMat <- unsafeWith cpuMat $ \ptr' -> do
        let ptr = ((), ptr')
        A.toPtr gpuMat ptr
        action $ MMatrix cpuMat gpuShape
        A.fromPtr gpuShape ptr :: IO (A.Array A.DIM2 a)
    return $ Raw newGpuMat

class Storable a => UnsafeIndexable m a where
    data family MatValue (m :: * -> *) a
    unsafeShapeIndex :: MMatrix m a -> A.DIM2 -> MatValue m a
    unsafeConstant :: a -> MatValue m a

{-# INLINE index #-}
index :: UnsafeIndexable m a => A.Boundary a -> MMatrix m a -> Cartesian.Point2 Int -> MatValue m a
index boundary mimage@MMatrix{..} (Cartesian.Point2 x y) =
    case boundary of
        A.Clamp      -> mimage `unsafeShapeIndex` (A.Z A.:. ((x `min` w1) `max` 0) A.:. ((y `min` h1) `max` 0))
        A.Mirror     -> mimage `unsafeShapeIndex` (A.Z A.:. (abs $ -abs (x `mod` (2 * width) - w1) + w1) A.:. (abs $ -abs (y `mod` (2 * height) - h1) + h1))
        A.Wrap       -> mimage `unsafeShapeIndex` (A.Z A.:. (x `mod` width) A.:. (y `mod` height))
        A.Constant a -> if (x >= width || y >= height || x < 0 || y < 0)
                        then unsafeConstant a
                        else mimage `unsafeShapeIndex` (A.Z A.:. x A.:. y)
    where A.Z A.:. height A.:. width = canvas
          h1 = height - 1 -- FIXME [KL]: Buggy behavior with index ranges
          w1 = width - 1

instance Storable a => UnsafeIndexable IOVector a where
    data MatValue IOVector a = MValue (IO a) (a -> IO ())
    unsafeShapeIndex MMatrix{..} sh = MValue value setter
        where linearIndex = Sugar.toIndex canvas sh
              value =  unsafeRead vector linearIndex
              setter = unsafeWrite vector linearIndex

    unsafeConstant a = MValue (return a) (const $ error "Unable to save to the Constant array value!")

class HasGetter a g | a -> g where
    get :: a -> g

class HasSetter a s | a -> s where
    set :: a -> s -> IO ()

    infixr 2 $=
    ($=) :: a -> s -> IO ()
    ($=) = set

instance HasGetter (MatValue IOVector a) (IO a) where
    get (MValue g _) = g

instance HasSetter (MatValue IOVector a) a where
    set (MValue _ s) = s
