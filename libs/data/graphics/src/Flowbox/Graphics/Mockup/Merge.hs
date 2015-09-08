---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------
module Flowbox.Graphics.Mockup.Merge (
  AlphaBlend(..),
  MergeMode(..),
  mergeLuna,
) where


import           Flowbox.Graphics.Composition.Merge (AlphaBlend (..))
import qualified Flowbox.Graphics.Composition.Merge as Merge
import           Flowbox.Graphics.Image.Image       (Image)
import qualified Flowbox.Graphics.Image.Image       as Image
import qualified Flowbox.Graphics.Image.Matte       as Matte
import qualified Flowbox.Graphics.Shader.Matrix     as Shader
import qualified Flowbox.Graphics.Shader.Rasterizer as Shader
import           Flowbox.Graphics.Utils.Utils
import           Flowbox.Prelude                    as P hiding (lookup)

import           Flowbox.Graphics.Mockup.Basic
import           Flowbox.Graphics.Mockup.Transform

import           Data.Array.Accelerate     (Exp, Z(..), (:.)(..))
import qualified Data.Array.Accelerate     as A
import           Linear                    (V2 (..))
import           Math.Coordinate.Cartesian (Point2 (..))
import           Math.Space.Space          (Grid (..))
import           Flowbox.Graphics.Shader.Shader as Shader

data MergeMode = Atop
           | Average AlphaBlend
           | ColorBurn AlphaBlend
           | ColorDodge AlphaBlend
           | ConjointOver
           | Copy AlphaBlend
           | Difference AlphaBlend
           | DisjointOver
           | DivideBySource AlphaBlend
           | DivideByDestination AlphaBlend
           | Exclusion AlphaBlend
           | From AlphaBlend
           | Geometric AlphaBlend
           | HardLight AlphaBlend
           | Hypot AlphaBlend
           | In
           | MergeMask
           | MergeMatte
           | MergeMax AlphaBlend
           | MergeMin AlphaBlend
           | Minus AlphaBlend
           | Multiply AlphaBlend
           | Out
           | Over
           | Overlay AlphaBlend
           | Plus AlphaBlend
           | Screen AlphaBlend
           | SoftLight AlphaBlend
           | SoftLightPegtop AlphaBlend
           | SoftLightIllusions AlphaBlend
           | SoftLightPhotoshop AlphaBlend
           | Stencil
           | Under
           | XOR
           deriving (Show)

-- img1 is background, img2 is foreground
mergeLuna :: MergeMode -> Image -> Image -> Maybe (Matte.Matte Float) -> Image
mergeLuna mode img1 img2 matte = case mode of
    Atop                           -> processMerge $ Merge.threeWayMerge             Merge.atop
    Average alphaBlend             -> processMerge $ Merge.threeWayMerge' alphaBlend Merge.average
    ColorBurn alphaBlend           -> processMerge $ Merge.threeWayMerge' alphaBlend Merge.colorBurn
    ColorDodge alphaBlend          -> processMerge $ Merge.threeWayMerge' alphaBlend Merge.colorDodge
    ConjointOver                   -> processMerge $ Merge.threeWayMerge             Merge.conjointOver
    Copy alphaBlend                -> processMerge $ Merge.threeWayMerge' alphaBlend Merge.copy
    Difference alphaBlend          -> processMerge $ Merge.threeWayMerge' alphaBlend Merge.difference
    DisjointOver                   -> processMerge $ Merge.threeWayMerge             Merge.disjointOver
    DivideBySource alphaBlend      -> processMerge $ Merge.threeWayMerge' alphaBlend Merge.divideBySrc
    DivideByDestination alphaBlend -> processMerge $ Merge.threeWayMerge' alphaBlend Merge.divideByDst
    Exclusion alphaBlend           -> processMerge $ Merge.threeWayMerge' alphaBlend Merge.exclusion
    From alphaBlend                -> processMerge $ Merge.threeWayMerge' alphaBlend Merge.from
    Geometric alphaBlend           -> processMerge $ Merge.threeWayMerge' alphaBlend Merge.geometric
    HardLight alphaBlend           -> processMerge $ Merge.threeWayMerge' alphaBlend Merge.hardLight
    Hypot alphaBlend               -> processMerge $ Merge.threeWayMerge' alphaBlend Merge.hypot
    In                             -> processMerge $ Merge.threeWayMerge             Merge.inBlend
    MergeMask                      -> processMerge $ Merge.threeWayMerge             Merge.withMask
    MergeMatte                     -> processMerge $ Merge.threeWayMerge             Merge.matte
    MergeMax alphaBlend            -> processMerge $ Merge.threeWayMerge' alphaBlend Merge.max
    MergeMin alphaBlend            -> processMerge $ Merge.threeWayMerge' alphaBlend Merge.min
    Minus alphaBlend               -> processMerge $ Merge.threeWayMerge' alphaBlend Merge.minus
    Multiply alphaBlend            -> processMerge $ Merge.threeWayMerge' alphaBlend Merge.multiply
    Out                            -> processMerge $ Merge.threeWayMerge             Merge.out
    Over                           -> processMerge $ Merge.threeWayMerge             Merge.over
    Overlay alphaBlend             -> processMerge $ Merge.threeWayMerge' alphaBlend Merge.overlayFun
    Plus alphaBlend                -> processMerge $ Merge.threeWayMerge' alphaBlend Merge.plus
    Screen alphaBlend              -> processMerge $ Merge.threeWayMerge' alphaBlend Merge.screen
    SoftLight alphaBlend           -> processMerge $ Merge.threeWayMerge' alphaBlend Merge.softLight
    SoftLightPegtop alphaBlend     -> processMerge $ Merge.threeWayMerge' alphaBlend Merge.softLightPegtop
    SoftLightIllusions alphaBlend  -> processMerge $ Merge.threeWayMerge' alphaBlend Merge.softLightIllusions
    SoftLightPhotoshop alphaBlend  -> processMerge $ Merge.threeWayMerge' alphaBlend Merge.softLightPhotoshop
    Stencil                        -> processMerge $ Merge.threeWayMerge             Merge.stencil
    Under                          -> processMerge $ Merge.threeWayMerge             Merge.under
    XOR                            -> processMerge $ Merge.threeWayMerge             Merge.xor
    where processMerge f = img'
              where (r, g, b, a) = f r1 g1 b1 r2 g2 b2 a1 a2
                    rm = mixImages r r1 matteShader
                    gm = mixImages g g1 matteShader
                    bm = mixImages b b1 matteShader
                    am = mixImages a a1 matteShader
                    view' = insertChannelFloats view [
                                ("rgba.r", Shader.rasterizer $ rm)
                              , ("rgba.g", Shader.rasterizer $ gm)
                              , ("rgba.b", Shader.rasterizer $ bm)
                              , ("rgba.a", Shader.rasterizer $ am)
                            ]
                    img' = Image.insertPrimary view' img1
          Right view = Image.lookupPrimary img1
          Grid width1 height1 = canvas r1
          Grid width2 height2 = canvas r2
          (r1, g1, b1, a1) = unsafeGetChannels img1 & over each (Shader.fromMatrix (A.Constant 0))
          (r2, g2, b2, a2) = unsafeGetChannels img2 & over each (Shader.transform toBottomLeft . Shader.fromMatrix (A.Constant 0))
          --(r2m, g2m, b2m, a2m) = case matte of
          --    Just m -> let (h,w) = unpackAccDims (height2, width2)
          --                  msh = Shader.bound A.Clamp $ Matte.matteToDiscrete h w m
          --                  in ((*)<$>r2<*>msh,(*)<$>g2<*>msh,(*)<$>b2<*>msh,(*)<$>a2<*>msh) -- {-- invert <$> --} Matte.matteToDiscrete h w m
          --    _      -> (r2, g2, b2, a2)

          matteShader = case matte of
              Just m -> let (h,w) = unpackAccDims (height1, width1)
                            msh = {-- Shader.bound A.Clamp $ --} Matte.matteToDiscrete h w m
                            in msh --Shader.transform toBottomLeft msh -- {-- invert <$> --} Matte.matteToDiscrete h w m
              _      -> Shader.unitShader (\_->1)
          toBottomLeft :: Point2 (Exp Int) -> Point2 (Exp Int)
          toBottomLeft pt = case pt of
                                    Point2 x y -> let offset = height1-height2 in Point2 x (y-offset) -- (y-height1+height2)
          mixImages :: (Applicative f, Floating a) => f a -> f a -> f a -> f a -- CartesianShader a -> CartesianShader a -> CartesianShader a -> CartesianShader a
          mixImages merged original mask = (+)<$>((*)<$>merged<*>mask)<*>((*)<$>original<*>fmap (1 -) mask)

