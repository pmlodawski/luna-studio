---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances  #-}

module Flowbox.Graphics.Composition.Generator.Gradient where

import Flowbox.Graphics.Utils.Accelerate hiding (head, last)
import Flowbox.Graphics.Shader.Shader
import Flowbox.Prelude                   as P hiding ((?), fromList) -- unfortunately required for the tick sorting


--import Data.Profunctor                   (Profunctor(..))

import Data.Array.Accelerate             as A
import Data.Array.Accelerate.Array.Sugar ()
import Data.Array.Accelerate.Smart       ()
import Data.Array.Accelerate.Tuple       ()
import Data.List                         (sort)
import Data.Typeable                     (Typeable)
import Math.Coordinate
import Math.Coordinate.Cartesian         as Cartesian hiding (x, y, w)
import Math.Metric                       hiding (metric)



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


-- == Gradient functions ==

colorMapper :: forall a b c x . (Elt a, Elt b, Elt c, IsFloating a, Num a, Ord a)
            => [Tick a b c] -> (Exp a -> Exp b -> Exp c -> Exp b -> Exp c -> Exp a) -> Shader x (Exp a) -> Shader x (Exp a)
colorMapper ticks weightFun shapeShader = Shader (canvas shapeShader) $ \pixel ->
    let zippedTicks = A.zip accticks $ A.tail accticks
        accticks    = A.use $ fromList (Z :. P.length ticksNorm) ticksNorm
        ticksNorm   = firstElem : sort ticks P.++ [lastElem]
        firstElem   = head ticks & position .~ -1e20 -- FIXME [KL]: Do something with this constants
        lastElem    = last ticks & position .~ 1e20

        gradPos = runShader shapeShader pixel

        findColor acc positions = (gradPos >=* aPos &&* gradPos A.<* nPos) ? (newColor, acc)
            where (actualPos, nextPos) = unlift positions :: (Exp (Tick a b c), Exp (Tick a b c))
                  aPos = unlift actualPos ^. position
                  aVal = unlift actualPos ^. value
                  aWei = unlift actualPos ^. weight

                  nPos = unlift nextPos ^. position
                  nVal = unlift nextPos ^. value
                  nWei = unlift nextPos ^. weight

                  tickPos = (gradPos - aPos) / (nPos - aPos)
                  newColor = weightFun tickPos aVal aWei nVal nWei

    in sfoldl findColor 0 index0 zippedTicks

radialShape :: (Num b, MetricCoord a Cartesian, Metric a b c) => a -> Shader b c
radialShape metric = unitShader $ \pixel -> distanceBase metric 0 pixel

circularShape :: (Num a, Metric Euclidean a b) => Shader a b
circularShape = radialShape Euclidean

diamondShape :: (Num a, Metric Taxicab a b) => Shader a b
diamondShape = radialShape Taxicab

squareShape :: (Num a, Metric Chebyshev a b) => Shader a b
squareShape  = radialShape Chebyshev

conicalShape :: (Elt a, IsFloating a) => CartesianShader (Exp a) (Exp a)
conicalShape = unitShader $ \pixel -> let res = 1 - Cartesian.uncurry atan2 pixel / (2 * pi)
                                         in min (res A.>* 1 ? (res - 1, res)) 1

linearShape :: Fractional a => CartesianShader a a
linearShape = unitShader $ \(Point2 x _) -> x

gaussianShape :: Floating a => CartesianShader a a
gaussianShape = unitShader $ \(Point2 x _) -> cos x
