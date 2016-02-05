---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE DeriveFunctor         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}

module Flowbox.Graphics.Shader.Shader where

import           Flowbox.Graphics.Prelude  hiding (transform)
import           Flowbox.Math.Index        (Boundable)
import qualified Flowbox.Math.Index        as Index

import           Control.Monad             (ap)
import           Data.Array.Accelerate     (Boundary, Elt, Exp, IsNum)
import qualified Data.Array.Accelerate     as A

import           Math.Coordinate.Cartesian (Point2 (..))
import qualified Math.Coordinate.Cartesian as Cartesian
import           Math.Space.Space



-- == Shader type ==

data Shader a b = Shader { canvas    :: Grid (Exp Int)
                         , runShader :: a -> b
                         } deriving Functor

type CartesianShader a = Shader (Point2 a)
type DiscreteShader    = CartesianShader (Exp Int)
type ContinuousShader  = CartesianShader (Exp Float)

unitShader :: (a -> b) -> Shader a b
unitShader = Shader 1

instance Applicative (Shader a) where
    pure a = Shader 1 $ const a
    Shader (Grid w1 h1) f <*> Shader (Grid w2 h2) gen =
        Shader (Grid (w1 `max` w2) (h1 `max` h2)) $ ap f gen

instance Profunctor Shader where
    lmap f (Shader cnv gen) = Shader cnv $ gen . f
    rmap = fmap

transform :: (a -> t) -> Shader t b -> Shader a b
transform = lmap

resize :: Grid (Exp Int) -> Shader a b -> Shader a b
resize cnv (Shader _ gen) = Shader cnv gen

canvasT :: (Grid (Exp Int) -> Grid (Exp Int)) -> Shader a b -> Shader a b
canvasT f (Shader cnv gen) = Shader (f cnv) gen

combineWith :: (b -> c -> d) -> Shader a b -> Shader a c -> Shader a d
combineWith f (Shader (Grid w1 h1) g) (Shader (Grid w2 h2) h) =
	Shader (Grid (w1 `max` w2) (h1 `max` h2)) (\p -> f (g p) (h p))

instance Boundable (DiscreteShader b) (Exp Int) b where
    unsafeIndex2D = runShader
    boundary      = canvas

bound :: Elt t => Boundary (Exp t) -> DiscreteShader (Exp t) -> DiscreteShader (Exp t)
bound b gen = Shader (canvas gen) (Index.boundedIndex2D b gen)

--TODO[KM]: use with caution - consecutive applications cause the y axs to invert yet again so it might have to be done after reading the image and before saving it
--          on the other hand this might cause inconsistencies between the way we store data in a matrix and in a shader (inside an image)
fixY :: (Elt a, IsNum a) => CartesianShader (Exp a) b -> CartesianShader (Exp a) b
fixY (Shader grid@(Grid _ h) f) = Shader grid $ \(Point2 x y) -> f $ Point2 x (A.fromIntegral h - y)
