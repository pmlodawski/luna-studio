---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE ViewPatterns        #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Flowbox.Graphics.Composition.Generators.Stencil where

import Flowbox.Prelude                                    as P hiding (filter)
import Flowbox.Graphics.Composition.Generators.Structures
import Flowbox.Graphics.Composition.Generators.Constant
import Flowbox.Math.Matrix                                as M hiding (stencil)

import qualified Data.Array.Accelerate                    as A
import           Math.Coordinate.Cartesian                (Point2(..))
import           Math.Space.Space

stencil :: forall a b c . (Elt a, IsNum a) => (Point2 c -> Point2 (Exp Int) -> Point2 b) 
                                           -> Grid (Exp Int) -> DiscreteGenerator (Exp a) 
                                           -> (Exp a -> Exp a -> Exp a) -> Exp a
                                           -> Generator b (Exp a) -> Generator c (Exp a)
stencil mode (Grid width height) (Generator kernel) foldOp initVal (Generator input) = Generator $ \pos -> 
    let get x' y' = input $ mode pos (Point2 x' y')
        outer :: (Exp Int, Exp a) -> (Exp Int, Exp a)
        outer (h, acc) = (h + 1, A.snd $ A.while (\e -> A.fst e A.<* width) (A.lift1 inner) (A.lift (0 :: Exp Int, acc)))
            where inner :: (Exp Int, Exp a) -> (Exp Int, Exp a)
                  inner (w, acc) = (w + 1, acc `foldOp` ((kernel $ Point2 w h) * (get (w - width `div` 2) (h - height `div` 2))))
    in A.snd $ A.while (\e -> A.fst e A.<* height) (A.lift1 outer) (A.lift (0 :: Exp Int, initVal))
