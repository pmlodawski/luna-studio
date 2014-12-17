---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE TypeOperators #-}

-- | Module intented to contain various numeric utilities found
--   in Matlab, Octave or other programs.
module Flowbox.Math.Numeric where

import qualified Data.Array.Accelerate as A

import Flowbox.Graphics.Utils.Accelerate
import Flowbox.Prelude                   hiding (head, last, pre, ignored, index)



cumsum :: (A.Elt e, A.IsNum e) => A.Acc (A.Vector e) -> A.Acc (A.Vector e)
cumsum = A.scanl1 (+)

diff :: (A.Elt e, A.IsNum e) => A.Acc (A.Vector e) -> A.Acc (A.Vector e)
diff x = A.zipWith (-) (A.tail x) x

matMul :: (A.IsNum e, A.Elt e)
       => A.Acc (A.Array A.DIM2 e) -> A.Acc (A.Array A.DIM2 e) -> A.Acc (A.Array A.DIM2 e)
matMul arr brr = A.fold (+) 0 $ A.zipWith (*) arrRepl brrRepl
  where A.Z A.:. rowsA A.:. _     = A.unlift (A.shape arr)    :: A.Z A.:. A.Exp Int A.:. A.Exp Int
        A.Z A.:. _     A.:. colsB = A.unlift (A.shape brr)    :: A.Z A.:. A.Exp Int A.:. A.Exp Int

        arrRepl = A.replicate (A.lift $ A.Z A.:. A.All A.:. colsB A.:. A.All) arr
        brrRepl = A.replicate (A.lift $ A.Z A.:. rowsA A.:. A.All A.:. A.All) (A.transpose brr)

-- | Given a condition and input vector, returns indices in original vector that satisfied
--   the condition.
find :: A.Elt e => (A.Exp e -> A.Exp Bool) -> A.Acc (A.Vector e) -> A.Acc (A.Vector Int)
find pre input = A.filter (A./=* bogusValue) $ A.scatterIf zeroToSizeInt input pre ignored zeroToSize
  where
    zeroToSizeInt = A.enumFromN inputSize (0 :: A.Exp Int)
    zeroToSize = A.enumFromN inputSize 0
    ignored    = A.fill inputSize bogusValue
    bogusValue = -1
    inputSize  = A.index1 $ A.size input

-- | Given vectors with X and Y coordinates, compute values at given points. Uses linear interpolation.
interp1 :: (A.Elt e, A.IsFloating e) => A.Acc (A.Vector e) -> A.Acc (A.Vector e) -> A.Acc (A.Vector e) -> A.Acc (A.Vector e)
interp1 x v = smap $ lininterp1 x v

lininterp1 :: (A.Elt e, A.IsFloating e)
           => A.Acc (A.Vector e) -> A.Acc (A.Vector e) -> A.Exp e -> A.Exp e
lininterp1 x v xi = lerp (v A.!! pindex) (v A.!! index) slope
  where
    pindex, index :: A.Exp Int
    pindex = last $ find (\x' -> xi A.>=* x') x
    index  = head $ find (\x' -> xi A.<=* x') x

    slope = (index A.==* pindex) A.? (0, (xi - xp) / ((x A.!! index) - xp))
    xp    = x A.!! pindex

lerp :: (A.Elt e, A.IsNum e) => A.Exp e -> A.Exp e -> A.Exp e -> A.Exp e
lerp a b t = (1 - t) * a + t * b
