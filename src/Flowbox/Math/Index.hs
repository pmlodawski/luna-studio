{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE RecordWildCards        #-}

module Flowbox.Math.Index where

import           Flowbox.Prelude       as P
import qualified Data.Array.Accelerate as A

import Math.Coordinate.Cartesian
import Math.Space.Space



-- == Boundable class
class Boundable a b c | a -> b c where
    unsafeIndex2D  :: a -> Point2 b -> c
    bounduary      :: a -> Grid b

-- Boundable matrix
data BMatrix m a = BMatrix { container :: m a
                           , canvas :: A.DIM2
                           }

boundedIndex2D :: (Boundable a b c, Integral b) => A.Boundary c -> a -> Point2 b -> c
boundedIndex2D b obj (Point2 x y) = case b of
    A.Clamp      -> getter $ Point2 ((x `min` (width - 1)) `max` 0) ((y `min` (height - 1)) `max` 0) 
    A.Wrap       -> getter $ Point2 (x `mod` width) (y `mod` height) 
    A.Mirror     -> getter $ Point2 0 0 -- TODO [KL]
    A.Constant a -> a
    where Grid width height = bounduary obj
          getter            = unsafeIndex2D obj
