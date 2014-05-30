---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE FlexibleContexts    #-}

module Flowbox.Graphics.Composition.Raster where

import           Data.Array.Accelerate (Exp)
import qualified Data.Array.Accelerate as A

import           Flowbox.Graphics.Channel   (ChannelAcc, Channel2)
import qualified Flowbox.Graphics.Channel   as Channel
import           Flowbox.Graphics.Color     (ColorConvertAcc)
import qualified Flowbox.Graphics.Color     as Color
--import           Flowbox.Graphics.Color.RGB     (RGB) -- FIXME: why this no work? =O
import qualified Flowbox.Graphics.Color.RGB as Color
import           Flowbox.Graphics.Image     (Image)
import qualified Flowbox.Graphics.Image     as Img
import qualified Flowbox.Graphics.Utils     as U
import           Flowbox.Prelude



constant :: (A.Elt a, A.IsFloating a, A.Shape ix, Image img (ChannelAcc ix a)) => Exp ix -> [(Channel.Name, Exp a)] -> img (ChannelAcc ix a)
constant sh = foldr appendChannel mempty
    where appendChannel (name, value) = Img.insert name (Channel.fill sh value)

--type CheckerboardColors a = (ColorAcc a, ColorAcc a, ColorAcc a, ColorAcc a)
--type CheckerboardLine a   = (ColorAcc a, Exp Double)
type CheckerboardColors f a = (f a, f a, f a, f a)
type CheckerboardLine f a   = (f a, Exp Double)

checkerboard :: (A.Elt a, A.IsFloating a, Image img (Channel2 a), ColorConvertAcc c Color.RGB)
     => Exp A.DIM2 -> Exp Double -> CheckerboardColors c (Exp a) -> CheckerboardLine c (Exp a) -> CheckerboardLine c (Exp a) -> img (Channel2 a)
checkerboard sh size (color1, color2, color3, color4) (lineColor, lineWidth) (lineColorCenter, lineWidthCenter)
    = foldr appendChannel mempty ["rgba.r", "rgba.g", "rgba.b", "rgba.a"]
    where
        appendChannel name = Img.insert name (Channel.generate sh (calculateValue name))
        A.Z A.:. (h :: Exp Int) A.:. (w :: Exp Int) = A.unlift sh
        w' = A.fromIntegral w :: Exp Double
        h' = A.fromIntegral h :: Exp Double
        calculateValue name idx =
            let
                A.Z A.:. (y :: Exp Int) A.:. (x :: Exp Int) = A.unlift idx
                x' = A.fromIntegral x :: Exp Double
                y' = A.fromIntegral y :: Exp Double
            in if name == "rgba.a"
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
        isLine x y = (x + lineThreshold) `U.nonIntRem` size A.<* lineWidth
                     A.||* (y + lineThreshold) `U.nonIntRem` size A.<* lineWidth
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
