---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns        #-}

module Flowbox.Graphics.Composition.Raster where

import           Data.Array.Accelerate (Exp)
import qualified Data.Array.Accelerate as A

import qualified Flowbox.Graphics.Color         as Color
import qualified Flowbox.Graphics.Image.Channel as Channel
import           Flowbox.Graphics.Image.Image   (Image)
import qualified Flowbox.Graphics.Image.Image   as Image
import qualified Flowbox.Graphics.Image.View    as View
import qualified Flowbox.Graphics.Utils         as Utils
import qualified Flowbox.Math.Matrix            as Matrix
import           Flowbox.Prelude



constant :: View.View v => Exp A.DIM2 -> [(Channel.Name, Exp Double)] -> Image v
constant sh = Image.singleton . foldr appendChannel (View.empty "rgba")
    where appendChannel (name, value) = View.append (Channel.ChannelFloat name . Channel.FlatData $ Matrix.fill sh value)

type CheckerboardColors f a = (f a, f a, f a, f a)
type CheckerboardLine f a = (f a, Exp Double)

checkerboard :: (View.View v, Color.ColorConvert c Color.RGB)
             => Exp A.DIM2 -> Exp Double -> CheckerboardColors c (Exp a)
             -> CheckerboardLine c (Exp a) -> CheckerboardLine c (Exp a)
             -> Image v
checkerboard sh size (color1, color2, color3, color4) (lineColor, lineWidth) (lineColorCenter, lineWidthCenter) =
    Image.singleton . foldr appendChannel (View.empty "rgba") ["rgba.r", "rgba.g", "rgba.b", "rgba.a"]
    where appendChannel name = View.append (Channel.ChannelFloat name . Channel.FlatData $ Matrix.generate sh (calculateValue name))
          
          A.Z A.:. h A.:. w = A.unlift sh :: Matrix.EDIM2
          w' = A.fromIntegral w
          h' = A.fromIntegral h

          calculateValue name (A.unlift -> A.Z A.:. y A.:. x :: Matrix.EDIM2) =
              let x' = A.fromIntegral x
                  y' = A.fromIntegral y
              in
                  if name == "rgba.a"
                      then A.constant 1
                      else A.cond (isCentralLine x' y') (getColor name lineColorCenter)
                         $ A.cond (isLine x' y')        (getColor name lineColor)
                         $ A.cond (isRect1 x' y')       (getColor name color1)
                         $ A.cond (isRect2 x' y')       (getColor name color2)
                         $ A.cond (isRect3 x' y')       (getColor name color3)
                         $ A.cond (isRect4 x' y')       (getColor name color4)
                         0

          getColor name color = case name of
                                    "rgba.r" -> r
                                    "rgba.g" -> g
                                    "rgba.b" -> b
                                    _        -> r
                                where Color.RGB r g b = Color.toRGB color

          isCentralLine x y = (x A.>=* centerX - centerThreshold A.&&* x A.<* centerX + centerThreshold)
                        A.||* (y A.>=* centerY - centerThreshold A.&&* y A.<* centerY + centerThreshold)

          isLine x y = (x + lineThreshold) `Utils.nonIntRem` size A.<* lineWidth
                 A.||* (y + lineThreshold) `Utils.nonIntRem` size A.<* lineWidth

          isRect1 x y = evenGrid x centerX A.&&* evenGrid y centerY
          isRect2 x y = oddGrid  x centerX A.&&* evenGrid y centerY
          isRect3 x y = oddGrid  x centerX A.&&* oddGrid  y centerY
          isRect4 x y = evenGrid x centerX A.&&* oddGrid  y centerY
          centerX = w' / 2
          centerY = h' / 2
          centerThreshold = lineWidthCenter / 2
          lineThreshold   = lineWidth / 2
          evenGrid a b    = A.even $ grid a b
          oddGrid a b     = A.odd $ grid a b
          grid a b        = (A.floor (a - b) :: Exp Int) `div` A.round size
