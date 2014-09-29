---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE DeriveFunctor         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# LANGUAGE FlexibleContexts      #-}

module Flowbox.Graphics.Composition.Generators.Structures where

import           Flowbox.Graphics.Prelude          hiding (transform, lift)
import qualified Flowbox.Prelude                   as P (Eq(..), Ord(..)) -- unfortunately required for the tick sorting
import           Flowbox.Graphics.Utils.Accelerate
import qualified Flowbox.Math.Index as I

import Data.Array.Accelerate
import Data.Array.Accelerate.Array.Sugar
import Data.Array.Accelerate.Smart
import Data.Array.Accelerate.Tuple
import Control.Monad (ap)
import Data.Typeable (Typeable)
import Data.Profunctor

import           Math.Coordinate.Coordinate
import qualified Math.Coordinate.Cartesian as Cartesian
import           Math.Space.Space

-- == Generator type ==

data Generator a b = Generator { canvas :: Grid (Exp Int)
                               , runGenerator :: a -> b
                               } deriving Functor

type CartesianGenerator a = Generator (Cartesian.Point2 a)
type DiscreteGenerator = CartesianGenerator (Exp Int)
type ContinousGenerator = CartesianGenerator (Exp Double)

unitGenerator :: (a -> b) -> Generator a b
unitGenerator = Generator 1

instance Applicative (Generator a) where
    pure a = Generator 1 $ const a
    Generator (Grid h1 w1) f <*> Generator (Grid h2 w2) gen =
        Generator (Grid (h1 `max` h2) (w1 `max` w2)) $ ap f gen

instance Profunctor Generator where
    lmap f (Generator cnv gen) = Generator cnv $ gen . f
    rmap = fmap

transform :: (a -> t) -> Generator t b -> Generator a b
transform = lmap

resize :: Grid (Exp Int) -> Generator a b -> Generator a b
resize cnv (Generator _ gen) = Generator cnv gen

canvasT :: (Grid (Exp Int) -> Grid (Exp Int)) -> Generator a b -> Generator a b
canvasT f (Generator cnv gen) = Generator (f cnv) gen

instance I.Boundable (DiscreteGenerator b) (Exp Int) b where
    unsafeIndex2D = runGenerator
    boundary     = canvas

-- == Gradient tick type ==

data Tick a b c = Tick { _position :: a
                       , _value    :: b
                       , _weight   :: c
                       } deriving (Show, Read, Typeable)

instance P.Eq a => P.Eq (Tick a b c) where
    Tick p1 _ _ == Tick p2 _ _ = p1 P.== p2

instance P.Ord a => P.Ord (Tick a b c) where
    compare (Tick p1 _ _) (Tick p2 _ _) = P.compare p1 p2

makeLenses ''Tick
deriveAccelerate ''Tick
