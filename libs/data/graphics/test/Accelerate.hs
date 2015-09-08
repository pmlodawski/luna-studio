---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TypeFamilies #-}

module Main where

import           Data.Array.Accelerate             as A
import qualified Flowbox.Graphics.Utils.Accelerate as UA
import           Flowbox.Prelude                   as P hiding ((#))

import qualified Data.Array.Accelerate.CUDA        as CUDA



main = do
    let v1    = A.use $ A.fromList (Z:.100) [1..100]
        v2    = A.use $ A.fromList (Z:.10)  [1..10]
        out :: A.Acc (A.Vector Int)
        out   = A.map (f v2) v1
        f :: A.Acc (A.Vector Int) -> A.Exp Int -> A.Exp Int
        f v x = A.sfoldl (+) x A.index0 $ UA.smap (+x) v

    print out
    print $ CUDA.run out

    return ()