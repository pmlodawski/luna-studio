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

import           Flowbox.Graphics.Prelude hiding (transform)
import qualified Flowbox.Math.Index       as I

import Control.Monad         (ap)
import Data.Array.Accelerate

import qualified Math.Coordinate.Cartesian as Cartesian
import           Math.Space.Space



-- == Shader type ==

data Shader a b = Shader { canvas    :: Grid (Exp Int)
                         , runShader :: a -> b
                         } deriving Functor

type CartesianShader a = Shader (Cartesian.Point2 a)
type DiscreteShader    = CartesianShader (Exp Int)
type ContinuousShader  = CartesianShader (Exp Float)

unitShader :: (a -> b) -> Shader a b
unitShader = Shader 1

instance Applicative (Shader a) where
    pure a = Shader 1 $ const a
    Shader (Grid h1 w1) f <*> Shader (Grid h2 w2) gen =
        Shader (Grid (h1 `max` h2) (w1 `max` w2)) $ ap f gen

instance Profunctor Shader where
    lmap f (Shader cnv gen) = Shader cnv $ gen . f
    rmap = fmap

transform :: (a -> t) -> Shader t b -> Shader a b
transform = lmap

resize :: Grid (Exp Int) -> Shader a b -> Shader a b
resize cnv (Shader _ gen) = Shader cnv gen

canvasT :: (Grid (Exp Int) -> Grid (Exp Int)) -> Shader a b -> Shader a b
canvasT f (Shader cnv gen) = Shader (f cnv) gen

combineWith :: (b -> b -> b) -> Shader a b -> Shader a b -> Shader a b
combineWith f (Shader (Grid h1 w1) g) (Shader (Grid h2 w2) h) =
	Shader (Grid (h1 `max` h2) (w1 `max` w2)) (\p -> f (g p) (h p))

instance I.Boundable (DiscreteShader b) (Exp Int) b where
    unsafeIndex2D = runShader
    boundary      = canvas
