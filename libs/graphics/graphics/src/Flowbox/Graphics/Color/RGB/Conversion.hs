---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}

module Flowbox.Graphics.Color.RGB.Conversion where

import           Data.Array.Accelerate as A

import           Flowbox.Graphics.Color.Conversion
import           Flowbox.Graphics.Color.RGB
import           Flowbox.Graphics.Color.RGBA
import           Flowbox.Graphics.Color.HSV
import           Flowbox.Graphics.Color.HSL
import           Flowbox.Graphics.Color.CMY
import           Flowbox.Graphics.Color.CMYK
import           Flowbox.Graphics.Color.YUV
import           Flowbox.Graphics.Color.YUV_HD
import           Flowbox.Graphics.Utils
import           Flowbox.Prelude



toRGB :: (Elt a, IsFloating a, ColorConvert c RGB) => c (Exp a) -> RGB (Exp a)
toRGB = convertColor

instance ColorConvert RGB RGB where
    convertColor = id

instance ColorConvert RGBA RGB where
    convertColor (RGBA r' g' b' _) = RGB r' g' b'

instance ColorConvert HSV RGB where
    convertColor (HSV  h' s' v') = RGB (r'+m') (g'+m') (b'+m')
        where (r', g', b') = unlift res
              res = helperHsvHsl i c' x
              h'' = h' * 6
              i = A.floor h'' `mod` 6 :: Exp (Plain Int)
              x = c' * (1 - abs(h'' `nonIntRem` 2 - 1))
              c' = v' * s'
              m' = v' - c'

instance ColorConvert HSL RGB where
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

instance ColorConvert YUV RGB where
    convertColor (YUV y' u' v') = RGB r' g' b'
        where r' = y' + 1.13983 * v'
              g' = y' - 0.39465 * u' - 0.58060 * v'
              b' = y' + 2.03211 * u'

instance ColorConvert YUV_HD RGB where
    convertColor (YUV_HD y' u' v') = RGB r' g' b'
        where r' = y' + 1.28033 * v'
              g' = y' - 0.21482 * u' - 0.38059 * v'
              b' = y' + 2.12798 * u'

-- = Helpers

helperHsvHsl :: (Elt a, Elt (Plain b), IsNum a, IsScalar a, Lift Exp b, Num b) => Exp a -> b -> b -> Exp (Plain b, Plain b, Plain b)
helperHsvHsl i x z = cond (i ==* 0) (lift (x,   z,   x*0))
                   $ cond (i ==* 1) (lift (z,   x,   x*0))
                   $ cond (i ==* 2) (lift (x*0, x,   z))
                   $ cond (i ==* 3) (lift (x*0, z,   x))
                   $ cond (i ==* 4) (lift (z,   x*0, x))
                   $ cond (i ==* 5) (lift (x,   x*0, z))
                   $ lift (x,   z,   x*0)
