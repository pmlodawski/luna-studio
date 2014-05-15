---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
module Flowbox.Graphics.Transform where

import qualified Data.Array.Repa                   as R
import qualified Data.Array.Repa.Algorithms.Matrix as M
import           Flowbox.Prelude                   hiding (lookup, map)

type RepaMatrix2 a = R.Array R.U R.DIM2 a
data Transformation = Transformation (RepaMatrix2 Double)
            deriving (Show)

data Transformed a = Transformed { src   :: a
                                 , trans :: Transformation
                                 }
           deriving (Show)

instance Functor Transformed where
    fmap f (Transformed s t) = Transformed (f s) t

transform :: a -> Transformed a
transform x = Transformed x mempty

translate :: Double -> Double -> Transformed a -> Transformed a
translate x y (Transformed img transposition) = Transformed img transposition'
    where transposition' = mappend t transposition
          t = Transformation (R.fromListUnboxed (R.Z R.:. 3 R.:. 3) [ 1, 0, -x
                                                                    , 0, 1, -y
                                                                    , 0, 0, 1 ])

rotate :: Double -> Transformed a -> Transformed a
rotate theta (Transformed img transposition) = Transformed img transposition'
    where transposition' = mappend t transposition
          t = Transformation (R.fromListUnboxed (R.Z R.:. 3 R.:. 3) [   cos (-theta) , sin (-theta), 0
                                                                    , -(sin (-theta)), cos (-theta), 0
                                                                    , 0           , 0        , 1])

rotateAt :: Double -> Double -> Double -> Transformed a -> Transformed a
rotateAt theta x y = translate (-x) (-y) . rotate theta . translate x y

scale :: Double -> Double -> Transformed a -> Transformed a
scale x y (Transformed img transposition) = Transformed img transposition'
    where transposition' = mappend t transposition
          t = Transformation (R.fromListUnboxed (R.Z R.:. 3 R.:. 3) [ 1/x, 0  , 0
                                                                    , 0  , 1/y, 0
                                                                    , 0  , 0  , 1])

scaleAt :: Double -> Double -> Double -> Double -> Transformed a -> Transformed a
scaleAt sx sy x y = translate (-x) (-y) . scale sx sy . translate x y


-- = Instances =

instance Monoid Transformation where
    mempty = Transformation (R.fromListUnboxed (R.Z R.:. 3 R.:. 3) [ 1, 0, 0
                                                                   , 0, 1, 0
                                                                   , 0, 0, 1])
    (Transformation a) `mappend` (Transformation b) = Transformation (M.mmultS a b)
