---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE ViewPatterns        #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Flowbox.Graphics.Composition.Generators.Transform where

import Flowbox.Prelude                                    as P hiding (transform, zoom)
import Flowbox.Math.Matrix                                as M
import Flowbox.Graphics.Composition.Generators.Structures

import qualified Data.Array.Accelerate     as A
import           Math.Coordinate.Cartesian (Point2(..))
import           Math.Space.Space
import           Linear.V2


-- == Rotation ==
turn' :: (Num a, Floating a) => a -> Point2 a -> Point2 a
turn' phi (Point2 x y) = Point2 x' y'
    where x' = cos phi * x - sin phi * y
          y' = sin phi * x + cos phi * y

turn :: (Num a, Floating a) => a -> CartesianGenerator a b -> CartesianGenerator a b
turn = transform . turn'

bbox :: (Elt a, Ord a, IsNum a, IsFloating a) => Exp a -> Grid (Exp Int) -> Grid (Exp Int)
bbox phi cnv = fmap A.ceiling $ Grid gw' gh'
    where Grid gw gh = fmap A.fromIntegral cnv
          pmax = Point2 (px1 `max` px2 `max` px3 `max` px4) (py1 `max` py2 `max` py3 `max` py4) 
          pmin = Point2 (px1 `min` px2 `min` px3 `min` px4) (py1 `min` py2 `min` py3 `min` py4) 
          Point2 gw' gh' = pmax - pmin
          Point2 px1 py1 = turn' phi $ Point2  0 0
          Point2 px2 py2 = turn' phi $ Point2 gw 0
          Point2 px3 py3 = turn' phi $ Point2 gw gh
          Point2 px4 py4 = turn' phi $ Point2 0  gh

rotate :: (Elt a, Num a, Ord a, IsFloating a) => Exp a -> CartesianGenerator (Exp a) b -> CartesianGenerator (Exp a) b
rotate phi gen = turn phi $ resize (bbox phi $ canvas gen) gen

-- :: (Elt b, IsFloating b) => Exp a -> CartesianGenerator (Exp b) b1 -> CartesianGenerator (Exp b) b1
rotateCenter :: (Elt b, IsFloating b, Ord b) => Exp b -> Generator (Point2 (Exp b)) b1 -> CartesianGenerator (Exp b) b1
rotateCenter phi gen@(Generator (Grid ow oh) _) = translate newcenter newgen 
    where oldcenter = V2 (-A.fromIntegral ow / 2) (-A.fromIntegral oh / 2)
          newgen@(Generator (Grid nw nh) _) = rotate phi $ translate oldcenter gen
          newcenter = V2 (A.fromIntegral nw / 2) (A.fromIntegral nh / 2)

-- == Translation ==
translate' :: Num a => V2 a -> Point2 a -> Point2 a
translate' (V2 dx dy) (Point2 x y) = Point2 (x - dx) (y - dy)

translate :: Num a => V2 a -> CartesianGenerator a b -> CartesianGenerator a b
translate = transform . translate'

-- == Scaling ==
zoom' :: (Num a, Fractional a) => V2 a -> Point2 a -> Point2 a
zoom' (V2 sx sy) (Point2 x y) = Point2 (x / sx) (y / sy)

zoom :: (Num a, Fractional a) => V2 a -> CartesianGenerator a b -> CartesianGenerator a b
zoom = transform . zoom'

scale :: (Elt a, IsNum a, IsFloating a) => V2 (Exp a) -> CartesianGenerator (Exp a) b -> CartesianGenerator (Exp a) b 
scale v gen = zoom v $ resize (Grid (sx * w) (sy * h)) gen
    where Grid w h = canvas gen
          V2 sx sy = fmap A.ceiling v

scaleTo :: (Elt a, IsNum a, IsFloating a) => Grid (Exp Int) -> CartesianGenerator (Exp a) b -> CartesianGenerator (Exp a) b 
scaleTo newCnv gen@(Generator oldCnv _) = resize newCnv $ zoom (V2 (nw / ow) (nh / oh)) gen
    where Grid nw nh = fmap A.fromIntegral newCnv
          Grid ow oh = fmap A.fromIntegral oldCnv
