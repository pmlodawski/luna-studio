---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns        #-}

module Flowbox.Graphics.Composition.Generator.Raster where

import           Data.Array.Accelerate (Exp)
import qualified Data.Array.Accelerate as A
import qualified Data.Map              as Map

import qualified Flowbox.Graphics.Color.Color   as Color
import qualified Flowbox.Graphics.Image.Channel as Channel
import           Flowbox.Graphics.Image.Image   (Image)
import qualified Flowbox.Graphics.Image.Image   as Image
import qualified Flowbox.Graphics.Image.View    as View
import qualified Flowbox.Graphics.Utils.Utils   as Utils
import qualified Flowbox.Math.Matrix            as Matrix
import           Flowbox.Prelude

data Format = PCVideo
            | NTSC
            | PAL
            | HD
            | NTSC169
            | PAL169
            | K1Super35
            | K1Cinemascope
            | K2Super35
            | K2Cinemascope
            | K4Super35
            | K4Cinemascope
            | Square256
            | Square512
            | Square1K
            | Square2K
            | CustomFormat Int Int deriving(Eq, Ord)

--instance Eq Format whe
formatMap = Map.fromList ([(PCVideo, (640, 480))
                         ,(NTSC, (720, 486))
                         ,(PAL, (720, 576))
                         ,(HD, (1920, 1080))
                         ,(NTSC169, (720, 486))
                         ,(PAL169, (720, 576))
                         ,(K1Super35, (1024, 778))
                         ,(K1Cinemascope, (914, 778))
                         ,(K2Super35, (2048, 1556))
                         ,(K2Cinemascope, (1828, 1556))
                         ,(K4Super35, (4096, 3112))
                         ,(K4Cinemascope, (3656, 3112))
                         ,(Square256, (256, 256))
                         ,(Square512, (512, 512))
                         ,(Square1K, (1024, 1024))
                         ,(Square2K, (2048, 2048))
                         --,(CustomFormat x y, ( x, y))
                         ] :: [(Format,(Int,Int))] )

constant :: Exp A.DIM2 -> [(Channel.Name, Exp Double)] -> Image
constant sh = Image.singleton . foldr appendChannel (View.emptyDefault)
    where appendChannel (name, value) = View.append (Channel.ChannelFloat name . Channel.MatrixData $ Matrix.fill sh value)

type CheckerboardColors f a = (f a, f a, f a, f a)
type CheckerboardLine f a = (f a, Exp Double)

checkerboard :: (Color.ColorConvert c Color.RGB)
             => Exp A.DIM2 -> Exp Double -> CheckerboardColors c (Exp Double)
             -> CheckerboardLine c (Exp Double) -> CheckerboardLine c (Exp Double)
             -> Image
checkerboard sh size (color1, color2, color3, color4) (lineColor, lineWidth) (lineColorCenter, lineWidthCenter) =
    Image.singleton $ foldr appendChannel (View.emptyDefault) ["rgba.r", "rgba.g", "rgba.b", "rgba.a"]
    where appendChannel name = View.append (Channel.ChannelFloat name . Channel.MatrixData $ Matrix.generate sh (calculateValue name))

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
