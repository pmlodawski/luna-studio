---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Flowbox.Graphics.Raster.Raster where

import           Data.Array.Accelerate  (Exp)
import qualified Data.Array.Accelerate  as A

import           Flowbox.Graphics.Color             (Color (..))
import qualified Flowbox.Graphics.Color             as Color
import qualified Flowbox.Graphics.Raster.Channel    as Channel
import           Flowbox.Graphics.Raster.Image      (ChannelName, ImageT)
import qualified Flowbox.Graphics.Raster.Image      as Img
import qualified Flowbox.Graphics.Utils             as U
import           Flowbox.Prelude                    as P



constant :: (A.Elt a, A.IsFloating a, A.Shape ix) => Exp ix -> [(ChannelName, Exp a)] -> ImageT ix a
constant sh = foldr appendChannel mempty
    where appendChannel (name, value) img = Img.insert name (Channel.generate sh (const value)) img



type CheckerboardColors a = (Color a, Color a, Color a, Color a)
type CheckerboardLine a = (Color a, Exp Double)

checkerboard :: (A.Elt a, A.IsFloating a)
     => Exp A.DIM2 -> Exp Double -> CheckerboardColors a -> CheckerboardLine a -> CheckerboardLine a -> ImageT A.DIM2 a
checkerboard sh size (color1, color2, color3, color4) (lineColor, lineWidth) (lineColorCenter, lineWidthCenter)
    = foldr appendChannel mempty ["rgba.r", "rgba.g", "rgba.b", "rgba.a"]
    where
        appendChannel name img = Img.insert name (Channel.generate sh (calculateValue name)) img
        (A.Z A.:. (h :: Exp Int) A.:. (w :: Exp Int)) = A.unlift sh
        w' = A.fromIntegral w :: Exp Double
        h' = A.fromIntegral h :: Exp Double
        calculateValue name ix =
            let
                (A.Z A.:. (y :: Exp Int) A.:. (x :: Exp Int)) = A.unlift ix
                x' = A.fromIntegral x :: Exp Double
                y' = A.fromIntegral y :: Exp Double
            in if name == "rgba.a"
                then A.constant 1
                else
                    isCentralLine x' y' A.? (getColor name lineColorCenter,
                    isLine x' y'        A.? (getColor name lineColor,
                    isRect1 x' y'       A.? (getColor name color1,
                    isRect2 x' y'       A.? (getColor name color2,
                    isRect3 x' y'       A.? (getColor name color3,
                    isRect4 x' y'       A.? (getColor name color4,
                    0))))))
        getColor name color =
            let RGB r g b = Color.toRGB color
            in case name of
                "rgba.r" -> r
                "rgba.g" -> g
                "rgba.b" -> b
        isCentralLine x y = (x A.>=* centerX - centerThreshold A.&&* x A.<* centerX + centerThreshold)
            A.||* (y A.>=* centerY - centerThreshold A.&&* y A.<* centerY + centerThreshold)
        isLine x y = (x + lineThreshold) `U.nonIntRem` size A.<* lineWidth
            A.||* (y + lineThreshold) `U.nonIntRem` size A.<* lineWidth
        isRect1 x y = evenGrid x centerX
            A.&&* evenGrid y centerY
        isRect2 x y = oddGrid x centerX
            A.&&* evenGrid y centerY
        isRect3 x y = oddGrid x centerX
            A.&&* oddGrid y centerY
        isRect4 x y = evenGrid x centerX
            A.&&* oddGrid y centerY
        centerX = w' / 2
        centerY = h' / 2
        centerThreshold = lineWidthCenter / 2
        lineThreshold = lineWidth / 2
        sizeThreshold = size / 2
        evenGrid a b = A.even $ grid a b
        oddGrid a b = A.odd $ grid a b
        grid a b = (A.floor (a - b) :: Exp Int) `div` (A.round size)
