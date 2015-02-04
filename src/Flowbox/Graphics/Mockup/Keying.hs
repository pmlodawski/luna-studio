---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------
{-# LANGUAGE ViewPatterns #-}

module Flowbox.Graphics.Mockup.Keying where

import qualified Data.Array.Accelerate as A

import qualified Flowbox.Graphics.Color.Color        as Color
import           Flowbox.Graphics.Composition.Keying (KeyerMode)
import qualified Flowbox.Graphics.Composition.Keying as Keying
import           Flowbox.Graphics.Image.Image        (Image)
import qualified Flowbox.Graphics.Image.Image        as Image
import           Flowbox.Graphics.Utils.Accelerate   (variable)
import qualified Flowbox.Math.Matrix                 as M
import           Flowbox.Prelude                     as P hiding (lookup)

import Flowbox.Graphics.Mockup.Basic

keyer' :: (A.Exp (Color.RGB Float) -> A.Exp Float) -> Image -> Image
keyer' f img = img'
    where rgb = unsafeGetRGB img
          Right view = Image.lookupPrimary img
          alpha = M.map f rgb

          view' = insertChannelFloats view [("rgba.a", alpha)]

          img' = Image.insertPrimary view' img

keyerLuna :: KeyerMode -> Float -> Float -> Float -> Float -> Image -> Image
keyerLuna mode (variable -> a) (variable -> b) (variable -> c) (variable -> d) img =
    keyer' (Keying.keyer mode (A.lift (a, b, c, d))) img

differenceKeyer' :: (A.Exp (Color.RGB Float) -> A.Exp (Color.RGB Float) -> A.Exp Float) -> Image -> Image -> Image
differenceKeyer' f background foreground = img'
    where backgroundRGB = unsafeGetRGB background
          foregroundRGB = unsafeGetRGB foreground

          alpha = M.map (A.uncurry f) $ M.zip backgroundRGB foregroundRGB

          Right view = Image.lookupPrimary foreground
          view' = insertChannelFloats view [("rgba.a", alpha)]

          img' = Image.insertPrimary view' foreground

differenceKeyerLuna :: Float -> Float -> Image -> Image -> Image
differenceKeyerLuna (variable -> offset) (variable -> gain) background foreground = img'
    where diff = Keying.differenceKeyer offset gain
          img' = differenceKeyer' diff background foreground
