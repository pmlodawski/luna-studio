---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE ConstraintKinds        #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE UndecidableInstances   #-}
{-# LANGUAGE ViewPatterns           #-}

module Flowbox.Graphics.Composition.Generators.Transform where

import Flowbox.Graphics.Composition.Generators.Structures
import Flowbox.Graphics.Prelude                           as P hiding (lifted, transform)
import Flowbox.Graphics.Utils.Linear
import Flowbox.Graphics.Utils
import Flowbox.Math.Matrix                                as M

import qualified Data.Array.Accelerate     as A
import           Math.Coordinate.Cartesian (Point2(..))
import           Math.Space.Space          hiding (width, height)
import           Linear                    hiding (normalize, inv33, rotate)



-- == Transformation classes ==
class Translate t a | t -> a where
    translate :: V2 a -> t -> t

class Rotate t a | t -> a where
    rotate :: a -> t -> t

class Scale s t where
    scale :: s -> t -> t

class CornerPin t a | t -> a where
    cornerPin :: (Point2 a, Point2 a, Point2 a, Point2 a) -> t -> t


-- == Instances for Point2 ==
instance Num a => Translate (Point2 a) a where
    translate (V2 dx dy) (Point2 x y) = Point2 (x - dx) (y - dy)

instance Floating a => Rotate (Point2 a) a where
    rotate phi (Point2 x y) = Point2 x' y'
        where x' = cos phi * x - sin phi * y
              y' = sin phi * x + cos phi * y

instance Fractional a => Scale (V2 a) (Point2 a) where
     scale (V2 sx sy) (Point2 x y) = Point2 (x / sx) (y / sy)


-- == Instances for CartesianGenerator ==
instance Num a => Translate (CartesianGenerator a b) a where
    translate = transform . translate

instance Floating a => Rotate (CartesianGenerator a b) a where
    rotate = transform . rotate

instance (Fractional a, a ~ a0) => Scale (V2 a0) (CartesianGenerator a b) where
    scale = transform . scale

instance (Elt a, IsFloating a, c ~ Exp Int) => Scale (Grid c) (CartesianGenerator (Exp a) b) where
    scale newCnv gen@(Generator oldCnv _) = resize newCnv $ scale (V2 (nw / ow) (nh / oh)) gen
        where Grid nw nh = fmap A.fromIntegral newCnv :: Grid (Exp a)
              Grid ow oh = fmap A.fromIntegral oldCnv :: Grid (Exp a)

instance (Elt a, IsFloating a, AccEpsilon a) => CornerPin (CartesianGenerator (Exp a) b) (Exp a) where
    cornerPin points gen@(Generator (asFloating -> cnv) _) = transform (cornerPin' cnv points) gen


-- == Instances for Grid (Exp Int) ==
instance (Condition a, Ord a, Floating a) => Rotate (Grid a) a where
    rotate phi = coveringGrid $ rotate phi

instance (Num a, a ~ a0) => Scale (V2 a0) (Grid a) where
    scale (V2 sx sy) (Grid gw gh) = Grid (gw * sx) (gh * sy)

instance (Elt a, IsFloating a, AccEpsilon a) => CornerPin (Grid (Exp a)) (Exp a) where
    cornerPin points grid = coveringGrid (cornerPin' grid points) grid


-- == Utils ==
onCenter :: ( a ~ Generator p1 r1, b ~ Generator p2 r2
            , Translate a (Exp res1), Translate b (Exp res2)
            , Elt res1, Elt res2, IsFloating res1, IsFloating res2
            ) => (a -> b) -> a -> b
onCenter f gen@(Generator (asFloating -> Grid ow oh) _) = translate newcenter newgen
    where oldcenter = V2 (-ow / 2) (-oh / 2)
          newgen@(Generator (asFloating -> Grid nw nh) _) = f $ translate oldcenter gen
          newcenter = V2 (nw / 2) (nh / 2)

coveringGrid :: (Num a, Num b, Condition b, Ord b)
             => (Point2 a -> Point2 b) -> Grid a -> Grid b
coveringGrid f (Grid gw gh) = Grid gw' gh'
    where pmax = Point2 (px1 `max` px2 `max` px3 `max` px4) (py1 `max` py2 `max` py3 `max` py4)
          pmin = Point2 (px1 `min` px2 `min` px3 `min` px4) (py1 `min` py2 `min` py3 `min` py4)
          Point2 gw' gh' = pmax - pmin
          Point2 px1 py1 = f $ Point2  0 0
          Point2 px2 py2 = f $ Point2 gw 0
          Point2 px3 py3 = f $ Point2 gw gh
          Point2 px4 py4 = f $ Point2 0  gh

cornerPin' :: forall a. (Elt a, IsFloating a, AccEpsilon a)
           => Grid (Exp a)
           -> (Point2 (Exp a), Point2 (Exp a), Point2 (Exp a), Point2 (Exp a))
           -> Point2 (Exp a) -> Point2 (Exp a)
cornerPin' (Grid width height) (Point2 x1 y1, Point2 x2 y2, Point2 x3 y3, Point2 x4 y4) (Point2 x y) = Point2 (hx / hz) (hy / hz)
    where V3 hx hy hz = matC !* V3 x y 1
          unsafeInv33 :: M33 (Exp a) -> M33 (Exp a)
          unsafeInv33 a = let (_, lifted) = A.unlift $ inv33 (A.lift a) :: (Exp Bool, Exp (M33 a))
                          in A.unlift <$> A.unlift lifted

          srcPlane = V3 (V3 0 width width )
                        (V3 0 0     height)
                        (V3 1 1     1     )

          V3 l1 u1 t1 = unsafeInv33 srcPlane !* V3 0 height 1
          matA = srcPlane !*! V3 (V3 l1 0  0 )
                                 (V3 0  u1 0 )
                                 (V3 0  0  t1)

          dstPlane = V3 (V3 x1 x2 x3)
                        (V3 y1 y2 y3)
                        (V3 1  1  1 )

          V3 l2 u2 t2 = unsafeInv33 dstPlane !* V3 x4 y4 1

          matB = dstPlane !*! V3 (V3 l2 0  0 )
                                 (V3 0  u2 0 )
                                 (V3 0  0  t2)

          matC = matA !*! unsafeInv33 matB
