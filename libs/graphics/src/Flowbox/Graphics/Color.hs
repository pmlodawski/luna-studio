---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
module Flowbox.Graphics.Color where

import           Data.Array.Accelerate (Exp)
import qualified Data.Array.Accelerate  as A

import           Flowbox.Graphics.Utils
import           Flowbox.Prelude        as P



data Color a = RGB  {r :: Exp a, g :: Exp a, b :: Exp a}
             | RGBA {r :: Exp a, g :: Exp a, b :: Exp a, a :: Exp a}
             | HSV  {h :: Exp a, s :: Exp a, v :: Exp a}
             | HSL  {h :: Exp a, s :: Exp a, v :: Exp a}
             | CMY  {c :: Exp a, m :: Exp a, y :: Exp a}
             | CMYK {c :: Exp a, m :: Exp a, y :: Exp a, k :: Exp a}
             deriving (Show)



toRGB :: (A.Elt a, A.IsFloating a) => Color a -> Color a
toRGB color@(RGB{}) = color
toRGB (RGBA r g b _) = RGB r g b
toRGB (HSV  h s v) = RGB (r+m) (g+m) (b+m)
    where (r, g, b) = A.unlift res
          res = i A.==* 0 A.? (A.lift (c,   x,   c*0),
                i A.==* 1 A.? (A.lift (x,   c,   c*0),
                i A.==* 2 A.? (A.lift (c*0, c,   x),
                i A.==* 3 A.? (A.lift (c*0, x,   c),
                i A.==* 4 A.? (A.lift (x,   c*0, c),
                i A.==* 5 A.? (A.lift (c,   c*0, x),
                               A.lift (c,   x,   c*0)
                              ))))))
          h' = h * 6
          i = (A.floor h') `mod` 6 :: Exp (A.Plain Int)
          x = c * (1 - abs(h' `nonIntRem` 2 - 1))
          c = v * s
          m = v - c
toRGB (HSL h s l) = RGB (r+m) (g+m) (b+m)
    where (r, g, b) = A.unlift res
          res = i A.==* 0 A.? (A.lift (c,   x,   c*0),
                i A.==* 1 A.? (A.lift (x,   c,   c*0),
                i A.==* 2 A.? (A.lift (c*0, c,   x),
                i A.==* 3 A.? (A.lift (c*0, x,   c),
                i A.==* 4 A.? (A.lift (x,   c*0, c),
                i A.==* 5 A.? (A.lift (c,   c*0, x),
                               A.lift (c,   x,   c*0)
                              ))))))
          h' = h * 6
          i = (A.floor h') `mod` 6 :: Exp (A.Plain Int)
          x = c * (1 - abs(h' `nonIntRem` 2 - 1))
          c = (1 - abs(2 * l - 1)) * s
          m = l - c / 2
toRGB (CMY c m y) = RGB r g b
    where r = 1 - c
          g = 1 - m
          b = 1 - y
toRGB (CMYK c m y k) = RGB r g b
    where r = (1 - c) * (1 - k)
          g = (1 - m) * (1 - k)
          b = (1 - y) * (1 - k)


toRGBA :: (A.Elt a, A.IsFloating a) => Color a -> Color a
toRGBA color@(RGBA{}) = color
toRGBA (RGB r g b) = RGBA r g b (r-r+1)
toRGBA color = toRGBA . toRGB $ color


toHSV :: (A.Elt a, A.IsFloating a) => Color a -> Color a
toHSV color@(HSV{}) = color
toHSV (RGB r g b) = HSV h' s v
    where h' = (h A.>* 0 A.? (h , h + 6)) / 6
          h = delta A.==* 0 A.? (0,
                r A.==* maxRGB A.? (((g - b) / delta) `nonIntRem` 6,
                g A.==* maxRGB A.? ((b - r) / delta + 2,
                (r-g) / delta + 4
              )))
          s = delta A.==* 0 A.? (0, delta / maxRGB)
          v = maxRGB
          minRGB = min r $ min g b
          maxRGB = max r $ max g b
          delta = maxRGB - minRGB
toHSV color = toHSV . toRGB $ color


toHSL :: (A.Elt a, A.IsFloating a) => Color a -> Color a
toHSL color@(HSL{}) = color
toHSL (RGB r g b) = HSL h' s l
    where h' = (h A.>* 0 A.? (h , h + 6)) / 6
          h = delta A.==* 0 A.? (0,
                r A.==* maxRGB A.? (((g - b) / delta) `nonIntRem` 6,
                g A.==* maxRGB A.? ((b - r) / delta + 2,
                (r-g) / delta + 4
              )))
          s = delta A.==* 0 A.? (0, delta / (1 - abs(2 * l - 1)))
          l = (maxRGB + minRGB) / 2
          minRGB = min r $ min g b
          maxRGB = max r $ max g b
          delta = maxRGB - minRGB
toHSL color = toHSL . toRGB $ color


toCMY :: (A.Elt a, A.IsFloating a) => Color a -> Color a
toCMY color@(CMY{}) = color
toCMY (RGB r g b) = CMY c m y
    where c = 1 - r
          m = 1 - g
          y = 1 - b
toCMY color = toCMY . toRGB $ color


toCMYK :: (A.Elt a, A.IsFloating a) => Color a -> Color a
toCMYK color@(CMYK{}) = color
toCMYK (RGB r g b) = CMYK c m y k
    where c = (1 - r - k) / (1 - k)
          m = (1 - g - k) / (1 - k)
          y = (1 - b - k) / (1 - k)
          k = 1 - maxRGB
          maxRGB = max r $ max g b
toCMYK color = toCMYK . toRGB $ color
