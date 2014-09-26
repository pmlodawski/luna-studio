---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns        #-}

module Flowbox.Graphics.Composition.Generators.Keyer where

import qualified Data.Array.Accelerate as A

import Flowbox.Graphics.Color
import Flowbox.Graphics.Utils (clamp')
import Flowbox.Prelude



type KeyerLine a = A.Exp (a, a, a, a)

data KeyerMode = Red
               | Green
               | Blue
               | Redscreen
               | Greenscreen
               | Bluescreen
               | Saturation
               | Luminance
               | Max
               | Min
               deriving Show

keyer :: forall a. (A.Elt a, A.IsFloating a)
      => KeyerMode
      -> KeyerLine a
      -> A.Exp (RGB a) -- ^ input color
      -> A.Exp a       -- ^ output alpha
keyer mode = A.lift2 $ keyer' mode
    where keyer' :: (A.Elt a, A.IsFloating a)
                 => KeyerMode
                 -> (A.Exp a, A.Exp a, A.Exp a, A.Exp a)
                 -> RGB (A.Exp a)
                 -> A.Exp a
          keyer' mode' (aVal, bVal, cVal, dVal) rgb@(RGB r g b) =
              let preClampedAlpha :: A.Exp a
                  preClampedAlpha = case mode' of
                      Red         -> r
                      Green       -> g
                      Blue        -> b
                      Redscreen   -> 1 - (r - max g b)
                      Greenscreen -> 1 - (g - max r b)
                      Bluescreen  -> 1 - (b - max r g)
                      Saturation  -> hslH $ toHSL rgb
                      Luminance   -> hslL $ toHSL rgb
                      Max         -> max r $ max g b
                      Min         -> min r $ min g b

                  risingEdgePosition :: A.Exp a -> A.Exp a
                  risingEdgePosition x = A.caseof aVal [
                                             ((A.==* bVal), 1)
                                           , ((A.==* 0),    0)
                                         ]
                                         ((1 / (bVal - aVal)) * x + (aVal / (aVal - bVal)))

                  fallingEdgePosition :: A.Exp a -> A.Exp a
                  fallingEdgePosition x = A.caseof dVal [
                                              ((A.==* cVal), 1)
                                            , ((A.==* 0),    0)
                                          ]
                                          ((1 / (cVal - dVal)) * x + (dVal / (dVal - cVal)))

                  alpha :: A.Exp a
                  alpha = A.caseof preClampedAlpha [
                              ((A.<=* aVal), 0)
                            , ((A.<=* bVal), risingEdgePosition preClampedAlpha)
                            , ((A.<=* cVal), 1)
                            , ((A.<=* dVal), fallingEdgePosition preClampedAlpha)
                          ]
                          ((cVal A.>=* 1) A.? (1, fallingEdgePosition preClampedAlpha))
              in clamp' 0 1 alpha

differenceKeyer :: forall a. (A.Elt a, A.IsFloating a)
                => A.Exp a -- ^ offset
                -> A.Exp a -- ^ gain
                -> A.Exp (RGB a) -- ^ background
                -> A.Exp (RGB a) -- ^ foreground
                -> A.Exp a
differenceKeyer offset gain = A.lift2 $ differenceKeyer' offset gain
    where differenceKeyer' :: A.Exp a
                           -> A.Exp a
                           -> RGB (A.Exp a)
                           -> RGB (A.Exp a)
                           -> A.Exp a
          differenceKeyer' offset' gain' (RGB bgR bgG bgB) (RGB fgR fgG fgB) =
              let rDistance = (fgR - bgR) ** 2
                  gDistance = (fgG - bgG) ** 2
                  bDistance = (fgB - bgB) ** 2

                  distance = rDistance + gDistance + bDistance
                  alpha    = distance * gain - offset
              in clamp' 0 1 alpha
