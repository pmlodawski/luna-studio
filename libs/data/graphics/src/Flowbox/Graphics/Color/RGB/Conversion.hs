---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# OPTIONS_GHC -fno-warn-orphans  #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts      #-}

module Flowbox.Graphics.Color.RGB.Conversion where

import           Data.Array.Accelerate as A

import           Flowbox.Graphics.Color.CMY
import           Flowbox.Graphics.Color.CMYK
import           Flowbox.Graphics.Color.Conversion
import           Flowbox.Graphics.Color.HSL
import           Flowbox.Graphics.Color.HSV
import           Flowbox.Graphics.Color.RGB
import           Flowbox.Graphics.Color.RGBA
import           Flowbox.Graphics.Color.YCbCr
import           Flowbox.Graphics.Color.YCbCr_HD
import           Flowbox.Graphics.Utils
import           Flowbox.Prelude



toRGB :: (Elt a, IsFloating a, ColorConvert c RGB) => c (Exp a) -> RGB (Exp a)
toRGB = convertColor

instance ColorConvert RGB RGB where
    convertColor = id

instance ColorConvert RGBA RGB where
    convertColor (RGBA r' g' b' _) = RGB r' g' b'

instance ColorConvert HSV RGB where
    convertColor (HSV h' s' v') = RGB (r'+m') (g'+m') (b'+m')
        where (r', g', b') = unlift res
              res = helperHsvHsl i c' x
              h'' = h' * 6
              i = A.floor h'' `mod` 6 :: Exp (Plain Int)
              x = c' * (1 - abs(h'' `nonIntRem` 2 - 1))
              c' = v' * s'
              m' = v' - c'

instance ColorConvert HSL RGB where
    -- NOTE[mm]: There are slight differences between Nuke and this formula. Probably Nuke uses another way
    --           of computing HSL that gives different values for particular colors.
    convertColor (HSL h' s' l') = RGB (r'+m') (g'+m') (b'+m')
        where (r', g', b') = unlift res
              res = helperHsvHsl i c' x
              h'' = h' * 6
              i = A.floor h'' `mod` 6 :: Exp (Plain Int)
              x = c' * (1 - abs(h'' `nonIntRem` 2 - 1))
              c' = (1 - abs(2 * l' - 1)) * s'
              m' = l' - c' / 2

instance ColorConvert CMY RGB where
    convertColor (CMY c' m' y') = RGB r' g' b'
        where r' = 1 - c'
              g' = 1 - m'
              b' = 1 - y'

instance ColorConvert CMYK RGB where
    convertColor (CMYK c' m' y' k') = RGB r' g' b'
        where r' = (1 - c') * k''
              g' = (1 - m') * k''
              b' = (1 - y') * k''
              k'' = 1 - k'

instance ColorConvert YCbCr RGB where
    convertColor (YCbCr y' cb cr) = RGB r' g' b'
        where r' = y' + cr
              g' = y' - (kby / kgy) * cb - (kry / kgy) * cr
              b' = y' + cb
              kry = 0.299
              kby = 0.114
              kgy = 1 - kry - kby

instance ColorConvert YCbCr_HD RGB where
    convertColor (YCbCr_HD y' cb cr) = RGB r' g' b'
        where r' = y' + cr
              g' = y' - (kby / kgy) * cb - (kry / kgy) * cr
              b' = y' + cb
              kry = 0.2126
              kby = 0.0722
              kgy = 1 - kry - kby

-- = Helpers

helperHsvHsl :: (Elt a, Elt (Plain b), IsNum a, IsScalar a, Lift Exp b, Num b) => Exp a -> b -> b -> Exp (Plain b, Plain b, Plain b)
helperHsvHsl i x z = caseof i ([
                         ((==* 0), (A.lift (x,   z,   x*0)))
                       , ((==* 1), (A.lift (z,   x,   x*0)))
                       , ((==* 2), (A.lift (x*0, x,   z)))
                       , ((==* 3), (A.lift (x*0, z,   x)))
                       , ((==* 4), (A.lift (z,   x*0, x)))
                       , ((==* 5), (A.lift (x,   x*0, z)))
                     ])
                     (A.lift (x,   z,   x*0))
