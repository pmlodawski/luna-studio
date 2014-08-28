---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances    #-}

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

import qualified Data.Array.Accelerate as A
import           Data.Array.Accelerate hiding (Scalar, Vector, (!), shape, fromList, toList, (++))

import Flowbox.Prelude hiding (use, (<*), (?), (++), ix)



data Matrix ix a = Raw (Array ix a)
                 | Delayed (Acc (Array ix a))
                 deriving (Show)

type Scalar  a = Matrix DIM0 a
type Vector  a = Matrix DIM1 a
type Matrix2 a = Matrix DIM2 a

type Backend ix a = Acc (Array ix a) -> Array ix a

-- == Instances ==
instance (Elt e) => Monoid (Vector e) where
    mempty = fromList (Z :. 0) []
    mappend a b = a ++ b

-- == Helpers ==

type EDIM1 =  DIM0 :. Exp Int
type EDIM2 = EDIM1 :. Exp Int
type EDIM3 = EDIM2 :. Exp Int
type EDIM4 = EDIM3 :. Exp Int

accMatrix :: (Elt a, Shape ix) => Matrix ix a -> Acc (Array ix a)
accMatrix mat = case mat of
    Raw     m -> A.use m
    Delayed m -> m

compute :: Backend ix a -> Matrix ix a -> Matrix ix a
compute backend mat = Raw $ compute' backend mat

compute' :: Backend ix a -> Matrix ix a -> Array ix a
compute' backend mat = case mat of
    Raw     m -> m
    Delayed m -> backend m

-- == Accessors ==

-- = Scalar reduction =
sfoldl :: (Slice sh, Shape sh, Elt a, Elt b) => (Exp a -> Exp b -> Exp a) -> Exp a -> Exp sh -> Matrix (sh :. Int) b -> Exp a
sfoldl f z ix mat = A.sfoldl f z ix (accMatrix mat)

-- = Indexing =

(!) :: (Shape ix, Elt e) => Matrix ix e -> Exp ix -> Exp e
(!) mat = (A.!) $ accMatrix mat

(!!) :: (Shape ix, Elt e) => Matrix ix e -> Exp Int -> Exp e
(!!) mat = (A.!!) $ accMatrix mat

the :: Elt e => Scalar e -> Exp e
the mat = A.the $ accMatrix mat

boundedIndex :: Elt e => Boundary (Exp e) -> Matrix2 e -> Exp DIM2 -> Exp e
boundedIndex b mat i = case b of
    Clamp      -> mat ! index2 ((y `min` h1) `max` 0) ((x `min` w1) `max` 0)
    Mirror     -> mat ! index2 (abs $ -abs (y `mod` (2 * height) - h1) + h1) (abs $ -abs (x `mod` (2 * width) - w1) + w1)
    Wrap       -> mat ! index2 (y `mod` height) (x `mod` width)
    Constant a -> (x >=* width ||* y >=* height ||* x <* 0 ||* y <* 0) ? (a, mat ! i)
    where Z :. height :. width = unlift $ shape mat
          Z :. y      :. x     = unlift i
          h1 = height - 1
          w1 = width - 1

-- = Shape Information =

empty :: (Shape ix, Elt e) => Matrix ix e -> Exp Bool
empty mat = A.null $ accMatrix mat

shape :: (Shape ix, Elt e) => Matrix ix e -> Exp ix
shape mat = A.shape $ accMatrix mat

size :: (Shape ix, Elt e) => Matrix ix e -> Exp Int
size mat = A.size $ accMatrix mat

shapeSize :: Shape ix => Exp ix -> Exp Int
shapeSize = A.shapeSize

-- = Extracting sub-arrays =

slice :: (Slice slix, Elt e) => Matrix (FullShape slix) e -> Exp slix -> Matrix (SliceShape slix) e
slice mat fragments = Delayed $ A.slice (accMatrix mat) fragments

take :: (Elt e) => Exp Int -> Vector e -> Vector e
take elems mat = Delayed $ A.take elems (accMatrix mat)

-- == Construction ==

-- = Introduction =

use :: (Elt a, Shape ix) => Matrix ix a -> Matrix ix a
use mat = Delayed $ accMatrix mat

unit :: Elt e => Exp e -> Scalar e
unit e = Delayed $ A.unit e


-- = Initialisation =

generate :: (Shape ix, Elt e) => Exp ix -> (Exp ix -> Exp e) -> Matrix ix e
generate sh f = Delayed $ A.generate sh f

replicate :: (Slice slix, Elt e)
    => Exp slix -> Matrix (SliceShape slix) e -> Matrix (FullShape slix) e
replicate sh m = Delayed $ A.replicate sh $ accMatrix m

fill :: (Shape ix, Elt e) => Exp ix -> Exp e -> Matrix ix e
fill sh x = Delayed $ A.fill sh x


-- = Enumeration =

enumFromN :: (Shape ix, Elt e, IsNum e) => Exp ix -> Exp e -> Matrix ix e
enumFromN sh n = Delayed $ A.enumFromN sh n

enumFromStepN :: (Shape ix, Elt e, IsNum e)
    => Exp ix -> Exp e -> Exp e -> Matrix ix e
enumFromStepN sh n s = Delayed $ A.enumFromStepN sh n s

-- = Pipelining =
infixl 1 >->
(>->) :: (Shape ixa, Shape ixb, Shape ixc, Elt a, Elt b, Elt c)
      => (Matrix ixa a -> Matrix ixb b) -> (Matrix ixb b -> Matrix ixc c) -> Matrix ixa a -> Matrix ixc c
(>->) a b mat = Delayed $ (ftrans a A.>-> ftrans b) (accMatrix mat)

ftrans :: (Shape ixa, Shape ixb, Elt a, Elt b) => (Matrix ixa a -> Matrix ixb b) -> Acc (Array ixa a) -> Acc (Array ixb b)
ftrans f a = accMatrix $ f (Delayed a)

-- = Concatenation =

(++) :: (Slice ix, Shape ix, Elt e) => Matrix (ix :. Int) e -> Matrix (ix :. Int) e -> Matrix (ix :. Int) e
(++) chan1 chan2 = Delayed $ accMatrix chan1 A.++ accMatrix chan2


-- == Modifying arrays ==

-- = Shape manipulation =

reshape :: (Shape ix, Shape ix', Elt e) => Exp ix -> Matrix ix' e -> Matrix ix e
reshape sh mat = Delayed $ A.reshape sh (accMatrix mat)

flatten :: (Elt a, Shape ix) => Matrix ix a -> Vector a
flatten mat = Delayed $ A.flatten (accMatrix mat)

-- = Permutations =

transpose :: Elt e => Matrix2 e -> Matrix2 e
transpose mat = Delayed $ A.transpose $ accMatrix mat

permute :: (Shape ix, Shape ix', Elt a)
        => (Exp a -> Exp a -> Exp a)
        -> Matrix ix' a
        -> (Exp ix -> Exp ix')
        -> Matrix ix a
        -> Matrix ix' a
permute combination defaults permutation array =
    Delayed $ A.permute combination (accMatrix defaults) permutation (accMatrix array)

-- == Element-wise operations ==

-- = Mapping =

map :: (Shape ix, Elt a, Elt b)
    => (Exp a -> Exp b) -> Matrix ix a -> Matrix ix b
map f mat = Delayed $ A.map f (accMatrix mat)

-- = Zipping =

zipWith :: (Shape ix, Elt a, Elt b, Elt c)
    => (Exp a -> Exp b -> Exp c)
    -> Matrix ix a
    -> Matrix ix b
    -> Matrix ix c
zipWith f mat1 mat2 = Delayed $ A.zipWith f (accMatrix mat1) (accMatrix mat2)

zipWith3 :: (Shape ix, Elt a, Elt b, Elt c, Elt d)
    => (Exp a -> Exp b -> Exp c -> Exp d)
    -> Matrix ix a
    -> Matrix ix b
    -> Matrix ix c
    -> Matrix ix d
zipWith3 f mat1 mat2 mat3 = Delayed $ A.zipWith3 f (accMatrix mat1) (accMatrix mat2) (accMatrix mat3)

zipWith4 :: (Shape ix, Elt a, Elt b, Elt c, Elt d, Elt e)
    => (Exp a -> Exp b -> Exp c -> Exp d -> Exp e)
    -> Matrix ix a
    -> Matrix ix b
    -> Matrix ix c
    -> Matrix ix d
    -> Matrix ix e
zipWith4 f mat1 mat2 mat3 mat4 = Delayed $ A.zipWith4 f (accMatrix mat1) (accMatrix mat2) (accMatrix mat3) (accMatrix mat4)

zipWith5 :: (Shape ix, Elt a, Elt b, Elt c, Elt d, Elt e, Elt f)
    => (Exp a -> Exp b -> Exp c -> Exp d -> Exp e -> Exp f)
    -> Matrix ix a
    -> Matrix ix b
    -> Matrix ix c
    -> Matrix ix d
    -> Matrix ix e
    -> Matrix ix f
zipWith5 f mat1 mat2 mat3 mat4 mat5 = Delayed $ A.zipWith5 f (accMatrix mat1) (accMatrix mat2) (accMatrix mat3) (accMatrix mat4) (accMatrix mat5)

zipWith6 :: (Shape ix, Elt a, Elt b, Elt c, Elt d, Elt e, Elt f, Elt g)
    => (Exp a -> Exp b -> Exp c -> Exp d -> Exp e -> Exp f -> Exp g)
    -> Matrix ix a
    -> Matrix ix b
    -> Matrix ix c
    -> Matrix ix d
    -> Matrix ix e
    -> Matrix ix f
    -> Matrix ix g
zipWith6 f mat1 mat2 mat3 mat4 mat5 mat6 = Delayed $ A.zipWith6 f (accMatrix mat1) (accMatrix mat2) (accMatrix mat3) (accMatrix mat4) (accMatrix mat5) (accMatrix mat6)

zipWith7 :: (Shape ix, Elt a, Elt b, Elt c, Elt d, Elt e, Elt f, Elt g, Elt h)
    => (Exp a -> Exp b -> Exp c -> Exp d -> Exp e -> Exp f -> Exp g -> Exp h)
    -> Matrix ix a
    -> Matrix ix b
    -> Matrix ix c
    -> Matrix ix d
    -> Matrix ix e
    -> Matrix ix f
    -> Matrix ix g
    -> Matrix ix h
zipWith7 f mat1 mat2 mat3 mat4 mat5 mat6 mat7 = Delayed $ A.zipWith7 f (accMatrix mat1) (accMatrix mat2) (accMatrix mat3) (accMatrix mat4) (accMatrix mat5) (accMatrix mat6) (accMatrix mat7)

zipWith8 :: (Shape ix, Elt a, Elt b, Elt c, Elt d, Elt e, Elt f, Elt g, Elt h, Elt i)
    => (Exp a -> Exp b -> Exp c -> Exp d -> Exp e -> Exp f -> Exp g -> Exp h -> Exp i)
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

zipWith9 :: (Shape ix, Elt a, Elt b, Elt c, Elt d, Elt e, Elt f, Elt g, Elt h, Elt i, Elt j)
    => (Exp a -> Exp b -> Exp c -> Exp d -> Exp e -> Exp f -> Exp g -> Exp h -> Exp i -> Exp j)
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


zip :: (Shape ix, Elt a, Elt b)
    => Matrix ix a
    -> Matrix ix b
    -> Matrix ix (a, b)
zip  mat1 mat2 = Delayed $ A.zip (accMatrix mat1) (accMatrix mat2)

zip3 :: (Shape ix, Elt a, Elt b, Elt c)
    => Matrix ix a
    -> Matrix ix b
    -> Matrix ix c
    -> Matrix ix (a, b, c)
zip3 mat1 mat2 mat3 = Delayed $ A.zip3 (accMatrix mat1) (accMatrix mat2) (accMatrix mat3)

zip4 :: (Shape ix, Elt a, Elt b, Elt c, Elt d)
    => Matrix ix a
    -> Matrix ix b
    -> Matrix ix c
    -> Matrix ix d
    -> Matrix ix (a, b, c, d)
zip4 mat1 mat2 mat3 mat4 = Delayed $ A.zip4 (accMatrix mat1) (accMatrix mat2) (accMatrix mat3) (accMatrix mat4)

zip5 :: (Shape ix, Elt a, Elt b, Elt c, Elt d, Elt e)
    => Matrix ix a
    -> Matrix ix b
    -> Matrix ix c
    -> Matrix ix d
    -> Matrix ix e
    -> Matrix ix (a, b, c, d, e)
zip5 mat1 mat2 mat3 mat4 mat5 = Delayed $ A.zip5 (accMatrix mat1) (accMatrix mat2) (accMatrix mat3) (accMatrix mat4) (accMatrix mat5)

zip6 :: (Shape ix, Elt a, Elt b, Elt c, Elt d, Elt e, Elt f)
    => Matrix ix a
    -> Matrix ix b
    -> Matrix ix c
    -> Matrix ix d
    -> Matrix ix e
    -> Matrix ix f
    -> Matrix ix (a, b, c, d, e, f)
zip6 mat1 mat2 mat3 mat4 mat5 mat6 = Delayed $ A.zip6 (accMatrix mat1) (accMatrix mat2) (accMatrix mat3) (accMatrix mat4) (accMatrix mat5) (accMatrix mat6)

zip7 :: (Shape ix, Elt a, Elt b, Elt c, Elt d, Elt e, Elt f, Elt g)
    => Matrix ix a
    -> Matrix ix b
    -> Matrix ix c
    -> Matrix ix d
    -> Matrix ix e
    -> Matrix ix f
    -> Matrix ix g
    -> Matrix ix (a, b, c, d, e, f, g)
zip7 mat1 mat2 mat3 mat4 mat5 mat6 mat7 = Delayed $ A.zip7 (accMatrix mat1) (accMatrix mat2) (accMatrix mat3) (accMatrix mat4) (accMatrix mat5) (accMatrix mat6) (accMatrix mat7)

zip8 :: (Shape ix, Elt a, Elt b, Elt c, Elt d, Elt e, Elt f, Elt g, Elt h)
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

zip9 :: (Shape ix, Elt a, Elt b, Elt c, Elt d, Elt e, Elt f, Elt g, Elt h, Elt i)
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

unzip :: (Shape ix, Elt a)
    => Matrix ix (a, a)
    -> (Matrix ix a, Matrix ix a)
unzip mat = over each Delayed $ A.unzip (accMatrix mat)

unzip3 :: (Shape ix, Elt a)
    => Matrix ix (a, a, a)
    -> (Matrix ix a, Matrix ix a, Matrix ix a)
unzip3 mat = over each Delayed $ A.unzip3 (accMatrix mat)

unzip4 :: (Shape ix, Elt a)
    => Matrix ix (a, a, a, a)
    -> (Matrix ix a, Matrix ix a, Matrix ix a, Matrix ix a)
unzip4 mat = over each Delayed $ A.unzip4 (accMatrix mat)

unzip5 :: (Shape ix, Elt a)
    => Matrix ix (a, a, a, a, a)
    -> (Matrix ix a, Matrix ix a, Matrix ix a, Matrix ix a, Matrix ix a)
unzip5 mat = over each Delayed $ A.unzip5 (accMatrix mat)

unzip6 :: (Shape ix, Elt a)
    => Matrix ix (a, a, a, a, a, a)
    -> (Matrix ix a, Matrix ix a, Matrix ix a, Matrix ix a, Matrix ix a, Matrix ix a)
unzip6 mat = over each Delayed $ A.unzip6 (accMatrix mat)

unzip7 :: (Shape ix, Elt a)
    => Matrix ix (a, a, a, a, a, a, a)
    -> (Matrix ix a, Matrix ix a, Matrix ix a, Matrix ix a, Matrix ix a, Matrix ix a, Matrix ix a)
unzip7 mat = over each Delayed $ A.unzip7 (accMatrix mat)

unzip8 :: (Shape ix, Elt a)
    => Matrix ix (a, a, a, a, a, a, a, a)
    -> (Matrix ix a, Matrix ix a, Matrix ix a, Matrix ix a, Matrix ix a, Matrix ix a, Matrix ix a, Matrix ix a)
unzip8 mat = over each Delayed $ A.unzip8 (accMatrix mat)

unzip9 :: (Shape ix, Elt a)
    => Matrix ix (a, a, a, a, a, a, a, a, a)
    -> (Matrix ix a, Matrix ix a, Matrix ix a, Matrix ix a, Matrix ix a, Matrix ix a, Matrix ix a, Matrix ix a, Matrix ix a)
unzip9 mat = over each Delayed $ A.unzip9 (accMatrix mat)


-- == Folding ==

fold :: (Shape ix, Elt a) => (Exp a -> Exp a -> Exp a) -> Exp a -> Matrix (ix :. Int) a -> Matrix ix a
fold f acc mat = Delayed $ A.fold f acc $ accMatrix mat

fold1 :: (Shape ix, Elt a) => (Exp a -> Exp a -> Exp a) -> Matrix (ix :. Int) a -> Matrix ix a
fold1 f mat = Delayed $ A.fold1 f $ accMatrix mat

foldAll :: (Shape sh, Elt a) => (Exp a -> Exp a -> Exp a) -> Exp a -> Matrix sh a -> Scalar a
foldAll f acc mat = Delayed $ A.foldAll f acc $ accMatrix mat

fold1All :: (Shape sh, Elt a) => (Exp a -> Exp a -> Exp a) -> Matrix sh a -> Scalar a
fold1All f mat = Delayed $ A.fold1All f $ accMatrix mat

foldSeg :: (Shape ix, Elt a, Elt i, IsIntegral i) => (Exp a -> Exp a -> Exp a) -> Exp a -> Matrix (ix :. Int) a -> Acc (Segments i) -> Matrix (ix :. Int) a
foldSeg f acc mat segments = Delayed $ A.foldSeg f acc (accMatrix mat) segments

fold1Seg :: (Shape ix, Elt a, Elt i, IsIntegral i) => (Exp a -> Exp a -> Exp a) -> Matrix (ix :. Int) a -> Acc (Segments i) -> Matrix (ix :. Int) a
fold1Seg f mat segments = Delayed $ A.fold1Seg f (accMatrix mat) segments

-- == Specialised folds ==

all :: (Shape sh, Elt e) => (Exp e -> Exp Bool) -> Matrix sh e -> Scalar Bool
all pred mat = Delayed $ A.all pred (accMatrix mat)

any :: (Shape sh, Elt e) => (Exp e -> Exp Bool) -> Matrix sh e -> Scalar Bool
any pred mat = Delayed $ A.any pred (accMatrix mat)

sum :: (IsNum a, Shape sh, Elt a) => Matrix sh a -> Scalar a
sum mat = Delayed $ A.sum (accMatrix mat)

product :: (IsNum a, Shape sh, Elt a) => Matrix sh a -> Scalar a
product mat = Delayed $ A.product (accMatrix mat)

minimum :: (IsNum a, Shape sh, Elt a) => Matrix sh a -> Scalar a
minimum mat = Delayed $ A.minimum (accMatrix mat)

maximum :: (IsNum a, Shape sh, Elt a) => Matrix sh a -> Scalar a
maximum mat = Delayed $ A.maximum (accMatrix mat)

-- == Stencil ==

stencil :: (Shape ix, Elt a, Elt b, Stencil ix a stencil)
    => (stencil -> Exp b) -> Boundary a -> Matrix ix a -> Matrix ix b
stencil f b mat = Delayed $ A.stencil f b (accMatrix mat)

stencil2 :: (Shape ix, Elt a, Elt b, Elt c, Stencil ix a stencil1, Stencil ix b stencil2)
    => (stencil1 -> stencil2 -> Exp c) -> Boundary a -> Matrix ix a -> Boundary b -> Matrix ix b -> Matrix ix c
stencil2 f b1 mat1 b2 mat2 = Delayed $ A.stencil2 f b1 (accMatrix mat1) b2 (accMatrix mat2)


-- == Operations ==

-- = Shape manipulation =

--index3 :: (Elt i, A.Slice (A.Z A.:. i), A.Slice (A.Z A.:. i A.:. i))
--    => Exp i -> Exp i -> Exp i -> Exp (A.Z A.:. i A.:. i A.:. i)
--index3 i j k = A.lift (A.Z A.:. i A.:. j A.:. k)

--lift2Dto3D :: (Elt a) => Matrix A.DIM2 a -> Matrix A.DIM3 a
--lift2Dto3D channel = reshape (index3 a b 1) channel
--    where (A.Z A.:. a A.:. b) = A.unlift $ shape channel

-- == Conversions ==

fromList :: (Shape sh, Elt e) => sh -> [e] -> Matrix sh e
fromList sh mat = Raw $ A.fromList sh mat

toList :: Backend sh e -> Matrix sh e -> [e]
toList b mat = A.toList $ compute' b mat
