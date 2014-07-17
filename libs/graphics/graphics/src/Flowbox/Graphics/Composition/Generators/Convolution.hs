---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE ViewPatterns        #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Flowbox.Graphics.Composition.Generators.Convolution where

import Flowbox.Prelude                                    as P hiding (filter)
import Flowbox.Graphics.Composition.Generators.Structures
import Flowbox.Math.Matrix                                as M

import qualified Data.Array.Accelerate                    as A
import           Math.Coordinate.Cartesian                (Point2(..))



convolve :: forall a b c . (Elt a, IsNum a) => (Point2 c -> Point2 (Exp Int) -> Point2 b) -> Matrix2 a -> Generator b (Exp a) -> Generator c (Exp a)
convolve mode kernel (Generator input) = Generator $ \pos -> 
    let get x' y' = input $ mode pos (Point2 x' y')
        Z :. height :. width = A.unlift $ shape kernel

        outer :: (Exp Int, Exp a) -> (Exp Int, Exp a)
        outer (h, acc) = (h + 1 , A.snd $ A.while (\e -> A.fst e A.<* width) (A.lift1 inner) (A.lift (0 :: Exp Int, acc)))
            where inner :: (Exp Int, Exp a) -> (Exp Int, Exp a)
                  inner (w, acc) = (w + 1, acc + (kernel M.! A.index2 h w) * (get (w - width `div` 2) (h - height `div` 2)))
    in A.snd $ A.while (\e -> A.fst e A.<* height) (A.lift1 outer) (A.lift (0 :: Exp Int, 0 :: Exp a))

filter :: (Elt a, IsNum a) => Exp Int -> Matrix2 a -> DiscreteGenerator (Exp a) -> DiscreteGenerator (Exp a)
filter scatter = convolve $ \point offset -> point + pure scatter * offset
