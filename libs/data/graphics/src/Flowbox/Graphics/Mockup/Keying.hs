---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------
{-# LANGUAGE ViewPatterns #-}

module Flowbox.Graphics.Mockup.Keying (
    differenceKeyerLuna,
    keyerLuna,
) where

import           Data.Array.Accelerate               ((:.) (..), Exp, Z (..))
import qualified Data.Array.Accelerate               as A
import           Math.Coordinate.Cartesian           (Point2 (..))

import           Flowbox.Geom2D.Rectangle            (Rectangle (..))
import qualified Flowbox.Graphics.Color.Color        as Color
import           Flowbox.Graphics.Composition.Keying (KeyerMode, KeyerThresholds (..))
import qualified Flowbox.Graphics.Composition.Keying as Keying
import           Flowbox.Graphics.Image.Image        (Image)
import qualified Flowbox.Graphics.Image.Image        as Image
import           Flowbox.Graphics.Utils.Accelerate   (variable)
import qualified Flowbox.Math.Matrix                 as M
import           Flowbox.Prelude                     as P hiding (lookup)

import           Flowbox.Graphics.Mockup.Basic
import           Flowbox.Graphics.Mockup.Transform

keyer' :: (A.Exp (Color.RGB Float) -> A.Exp Float) -> Image -> Image
keyer' f img = img'
    where rgb = unsafeGetRGB img
          Right view = Image.lookupPrimary img
          alpha = M.map f rgb

          view' = insertChannelFloats view [("rgba.a", alpha)]

          img' = Image.insertPrimary view' img

keyerLuna :: KeyerMode -> KeyerThresholds Float -> Image -> Image
keyerLuna mode (fmap variable -> KeyerThresholds a b c d) img =
    keyer' (Keying.keyer mode (A.lift (a, b, c, d))) img

differenceKeyer' :: (A.Exp (Color.RGB Float) -> A.Exp (Color.RGB Float) -> A.Exp Float) -> Bool -> Image -> Image -> Image
differenceKeyer' f constantOutside background foreground = img'
    where backgroundRGB = unsafeGetRGB $ cropLuna (Rectangle (Point2 0 0) (Point2 w h)) True constantOutside background
          foregroundRGB = unsafeGetRGB foreground
          Z :. h :. w   = A.unlift $ M.shape foregroundRGB

          alpha = M.map (A.uncurry f) $ M.zip backgroundRGB foregroundRGB

          Right view = Image.lookupPrimary foreground
          view' = insertChannelFloats view [("rgba.a", alpha)]

          img' = Image.insertPrimary view' foreground

differenceKeyerLuna :: Float -> Float -> Bool -> Image -> Image -> Image
differenceKeyerLuna (variable -> offset) (variable -> gain) constantOutside background foreground = img'
    where diff = Keying.differenceKeyer offset gain
          img' = differenceKeyer' diff constantOutside background foreground
