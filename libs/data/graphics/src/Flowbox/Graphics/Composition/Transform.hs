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
{-# LANGUAGE PatternSynonyms        #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE UndecidableInstances   #-}
{-# LANGUAGE ViewPatterns           #-}

module Flowbox.Graphics.Composition.Transform where

import           Flowbox.Geom2D.Rectangle
import           Flowbox.Graphics.Image.Channel    (ChannelData (..))
import qualified Flowbox.Graphics.Image.Channel    as Channel
import           Flowbox.Graphics.Prelude          as P hiding (lifted, transform)
import qualified Flowbox.Graphics.Shader.Matrix    as Shader
import           Flowbox.Graphics.Shader.Sampler   as Sampler
import           Flowbox.Graphics.Shader.Shader    (CartesianShader, Shader(..))
import qualified Flowbox.Graphics.Shader.Shader    as Shader
import           Flowbox.Graphics.Utils.Accelerate
import           Flowbox.Graphics.Utils.Linear
import           Flowbox.Math.Matrix               as M

import qualified Data.Array.Accelerate     as A
import           Linear                    hiding (inv33, normalize, rotate)
import           Math.Coordinate.Cartesian (Point2 (..))
import           Math.Space.Space          hiding (height, width)



-- == Transformation classes ==
class Translate t a | t -> a where
    translate :: V2 a -> t -> t

class Rotate t a | t -> a where
    rotate :: a -> t -> t

class Scale s t where
    scale :: s -> t -> t

class HorizontalSkew s t where
    horizontalSkew :: s -> t -> t

class VerticalSkew s t where
    verticalSkew :: s -> t -> t

class CornerPin t a | t -> a where
    cornerPin :: (Point2 a, Point2 a, Point2 a, Point2 a) -> t -> t

-- == Instances for Point2 ==
instance Num a => Translate (Point2 a) a where
    translate (V2 dx dy) (Point2 x y) = Point2 (x - dx) (y - dy)

instance Floating a => Rotate (Point2 a) a where
    rotate phi (Point2 x y) = Point2 x' y'
        where x' = cos phi * x - sin phi * y
              y' = sin phi * x + cos phi * y

--instance  Rotate (Point2 (A.Exp Double)) (A.Exp Double) where
--    rotate phi (Point2 x y) = Point2 x' y'
--        where x' = cos phi * x - sin phi * y
--              y' = sin phi * x + cos phi * y

instance Fractional a => Scale (V2 a) (Point2 a) where
    scale (V2 sx sy) (Point2 x y) = Point2 (x / sx) (y / sy)

instance Fractional a => HorizontalSkew a (Point2 a) where
    horizontalSkew k (Point2 x y) = Point2 (x+k*y) y

instance Fractional a => VerticalSkew a (Point2 a) where
    verticalSkew k (Point2 x y) = Point2 x (y+x*k)

-- == Instances for CartesianShader ==
instance Num a => Translate (CartesianShader a b) a where
    translate = Shader.transform . translate

instance Floating a => Rotate (CartesianShader a b) a where
    rotate = Shader.transform . rotate

instance (Fractional a, a ~ a0) => Scale (V2 a0) (CartesianShader a b) where
    scale = Shader.transform . scale

instance (Elt a, IsFloating a, c ~ Exp Int) => Scale (Grid c) (CartesianShader (Exp a) b) where
    scale newCnv gen@(Shader oldCnv _) = Shader.resize newCnv $ scale (V2 (nw / ow) (nh / oh)) gen
        where Grid nw nh = fmap A.fromIntegral newCnv :: Grid (Exp a)
              Grid ow oh = fmap A.fromIntegral oldCnv :: Grid (Exp a)

instance (Elt a, IsFloating a, AccEpsilon a) => CornerPin (CartesianShader (Exp a) b) (Exp a) where
    cornerPin points gen@(Shader (asFloating -> cnv) _) = Shader.transform (cornerPin' cnv points) gen


-- == Instances for Grid (Exp Int) ==
instance (Condition a, Ord a, Floating a) => Rotate (Grid a) a where
    rotate phi = coveringGrid $ rotate phi

instance (Num a, a ~ a0) => Scale (V2 a0) (Grid a) where
    scale (V2 sx sy) (Grid gw gh) = Grid (gw * sx) (gh * sy)

instance (Elt a, IsFloating a, AccEpsilon a) => CornerPin (Grid (Exp a)) (Exp a) where
    cornerPin points grid = coveringGrid (cornerPin' grid points) grid


-- == Utils ==
onCenter :: ( a ~ Shader p1 r1, b ~ Shader p2 r2
            , Translate a (Exp res1), Translate b (Exp res2)
            , Elt res1, Elt res2, IsFloating res1, IsFloating res2
            ) => (a -> b) -> a -> b
onCenter f gen@(Shader (asFloating -> Grid ow oh) _) = translate newcenter newgen
    where oldcenter = V2 (-ow / 2) (-oh / 2)
          newgen@(Shader (asFloating -> Grid nw nh) _) = f $ translate oldcenter gen
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

cornerPinShaderFrom points gen@(Shader (asFloating -> cnv) _) = Shader.transform (cornerPinFrom' cnv points) gen

-- FIXME[MM]: I know it's bad to copy this, but it's fast
--            the only difference is srcPlane <-> dstPlane exchange
cornerPinFrom' :: forall a. (Elt a, IsFloating a, AccEpsilon a)
           => Grid (Exp a)
           -> (Point2 (Exp a), Point2 (Exp a), Point2 (Exp a), Point2 (Exp a))
           -> Point2 (Exp a) -> Point2 (Exp a)
cornerPinFrom' (Grid width height) (Point2 x1 y1', Point2 x2 y2', Point2 x3 y3', Point2 x4 y4') (Point2 x y) = Point2 (hx / hz) (hy / hz)
    where V3 hx hy hz = matC !* V3 x y 1

          y1 = height - y1'
          y2 = height - y2'
          y3 = height - y3'
          y4 = height - y4'

          unsafeInv33 :: M33 (Exp a) -> M33 (Exp a)
          unsafeInv33 a = let (_, lifted) = A.unlift $ inv33 (A.lift a) :: (Exp Bool, Exp (M33 a))
                          in A.unlift <$> A.unlift lifted

          dstPlane = V3 (V3 0 width width )
                        (V3 0 0     height)
                        (V3 1 1     1     )

          V3 l1 u1 t1 = unsafeInv33 srcPlane !* V3 0 height 1
          matA = srcPlane !*! V3 (V3 l1 0  0 )
                                 (V3 0  u1 0 )
                                 (V3 0  0  t1)

          srcPlane = V3 (V3 x1 x2 x3)
                        (V3 y1 y2 y3)
                        (V3 1  1  1 )

          V3 l2 u2 t2 = unsafeInv33 dstPlane !* V3 x4 y4 1

          matB = dstPlane !*! V3 (V3 l2 0  0 )
                                 (V3 0  u2 0 )
                                 (V3 0  0  t2)

          matC = matA !*! unsafeInv33 matB

cornerPin' :: forall a. (Elt a, IsFloating a, AccEpsilon a)
           => Grid (Exp a)
           -> (Point2 (Exp a), Point2 (Exp a), Point2 (Exp a), Point2 (Exp a))
           -> Point2 (Exp a) -> Point2 (Exp a)
cornerPin' (Grid width height) (Point2 x1 y1', Point2 x2 y2', Point2 x3 y3', Point2 x4 y4') (Point2 x y) = Point2 (hx / hz) (hy / hz)
    where V3 hx hy hz = matC !* V3 x y 1

          y1 = height - y1'
          y2 = height - y2'
          y3 = height - y3'
          y4 = height - y4'
          
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

crop :: Elt a => Rectangle Int -> Bool -> Bool -> Exp a -> ChannelData a -> ChannelData a
crop (fmap variable . properRect -> Rect xA yA xB yB)
      reformat defaultOutside (defaultValue) chanData =
        case reformat of
          True -> processReformat
          False -> processStandard
        where
          processReformat = DiscreteData $ translate (V2 (-xA) (yB - h)) (Shader (Grid (xB - xA) (yB - yA)) s)
            where
              DiscreteData shader = Channel.asDiscreteData defaultValue chanData
              Shader (Grid w h) s = if defaultOutside then (Shader.bound (A.Constant defaultValue) shader) else (Shader.bound A.Clamp shader)

          processStandard = MatrixData $
            case defaultOutside of
              True -> M.generate newShape genConstOutside
              False -> M.generate newShape genClampedOutside
            where
              MatrixData matrix = Channel.asMatrixData chanData
              newShape = M.shape matrix
              clamp (y,x) = ((y `min` (h - yA - 1)) `max` (h - yB), (x `min` (xB - 1)) `max` xA)
              inside y x = (x A.>=* xA) A.&&* (x A.<* xB) A.&&* (h - yB A.<=* y) A.&&* (h - yA A.>* y)
              disjoint = A.not $ xA A.<* h A.&&* xB A.>* 0 A.&&* yA A.<* w A.&&* yB A.>* 0
              A.Z A.:. h A.:. w = A.unlift newShape :: M.EDIM2

              genConstOutside (A.unlift -> (Z :. y :. x)) = A.cond (inside y x) (matrix M.! A.index2 y x) defaultValue
              genClampedOutside (A.unlift -> (Z :. y :. x)) = A.cond (inside y x) (matrix M.! A.index2 y x) (A.cond disjoint defaultValue (proc (y,x)))
                where
                  proc (y, x) = matrix M.! A.index2 y' x'
                  (y',x') = clamp (y,x)

type CropReformat          = Bool
type CropConstantOutside a = Maybe (Exp a)

-- TODO[KM]: boundary, since it's a result of a case, might cause the kernel to recompile, check this and pack it in a variable if necessary
--           and the same MIGHT happen when handling the reformat (necessary values should be put inside a variable)
crop' :: Elt a => Rectangle (Exp Int) -> CropReformat -> CropConstantOutside a -> ChannelData a -> ChannelData a
crop' (properRect -> Rect xA yA xB yB) reformat constantOutside chanData = DiscreteData $ Shader.fixY out
    where boundary = case constantOutside of
              Nothing  -> A.Clamp
              Just val -> A.Constant val

          -- Shorter, but ugiler
          --DiscreteData discrete = Channel.asDiscreteData anyDummyValue chanData
          --Shader gridIn@(Grid inW inH) boundRun = Shader.fixY $ Shader.bound boundary discrete
          Shader gridIn@(Grid inW inH) boundRun = Shader.fixY $ case chanData of
              MatrixData m     -> Shader.fromMatrix boundary m
              DiscreteData d   -> Shader.bound boundary d
              ContinuousData d -> Shader.bound boundary $ Sampler.monosampler d

          Shader _ intersectedRun = let
                  interW    = (min inW xB - max 0 xA) `max` 1
                  interH    = (min inH yB - max 0 yA) `max` 1
                  interGrid = Grid interW interH
                  adjust    = V2 (min (-xA) 0) (min (-yA) 0)
              in Shader.bound boundary $ translate adjust $ Shader interGrid boundRun

          out = let
                  adjust x y = V2 (max x 0) (max y 0)
              in if reformat
                  then translate (adjust (-xA) (-yA)) $ Shader (Grid (xB - xA) (yB - yA)) intersectedRun
                  else translate (adjust   xA    yA)  $ Shader gridIn intersectedRun
