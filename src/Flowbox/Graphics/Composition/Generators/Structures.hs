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

import           Flowbox.Prelude    hiding (transform, lift)
import           Flowbox.Graphics.Utils.Accelerate
import qualified Flowbox.Math.Index as I

import Data.Array.Accelerate
import Data.Array.Accelerate.Array.Sugar
import Data.Array.Accelerate.Smart
import Data.Array.Accelerate.Tuple
import Data.Typeable                     (Typeable)
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

instance Eq a => Eq (Tick a b c) where
    Tick p1 _ _ == Tick p2 _ _ = p1 == p2

instance Ord a => Ord (Tick a b c) where
    compare (Tick p1 _ _) (Tick p2 _ _) = compare p1 p2

makeLenses ''Tick
deriveAccelerate ''Tick
