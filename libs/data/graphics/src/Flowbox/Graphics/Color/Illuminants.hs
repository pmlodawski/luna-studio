---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}

module Flowbox.Graphics.Color.Illuminants where

import Data.Array.Accelerate as A
import Data.Typeable

import Flowbox.Graphics.Color.CIE.XyY
import Flowbox.Graphics.Color.CIE.XYZ
import Flowbox.Graphics.Utils.Accelerate
import Flowbox.Prelude



data Chromaticity a = Chromaticity { chromaX :: a, chromaY :: a }
                    deriving (Show, Typeable)

deriveAccelerate ''Chromaticity


toxyY :: (Num a) => Chromaticity a -> XyY a
toxyY (Chromaticity x y) = XyY x y 1

toXYZ :: forall t. (A.Elt t, A.IsFloating t)
      => XyY (A.Exp t) -> XYZ (A.Exp t)
toXYZ (XyY x y y') = A.unlift $ A.cond (y A.==* 0)
    (A.lift $ XYZ 0 0 (0 :: A.Exp t) :: A.Exp (XYZ t))
    (A.lift   XYZ { xyzX = (x * y') / y
                  , xyzY = y'
                  , xyzZ = ((1 - x - y) * y') / y
                  } :: A.Exp (XYZ t)
    ) :: XYZ (A.Exp t)

toXYZ' :: (Num t, Eq t, Fractional t) => XyY t -> XYZ t
toXYZ' (XyY x y y') = if y' == 0 then XYZ 0 0 0 else XYZ { xyzX = (x * y') / y
                                                         , xyzY = y'
                                                         , xyzZ = ((1 - x - y) * y') / y
                                                         }

xyzColor :: (Illuminant a (A.Exp t), A.Elt t, A.IsFloating t)
         => a -> XYZ (A.Exp t)
xyzColor = toXYZ . toxyY . primaries



class Illuminant a b where
    primaries :: (Num b, Floating b) => a -> Chromaticity b

data A = A
       deriving (Show, Typeable)

instance Illuminant A n where
    primaries _ = Chromaticity 0.44757 0.40745

data B = B
       deriving (Show, Typeable)

instance Illuminant B n where
    primaries _ = Chromaticity 0.34842 0.35161

data C = C
       deriving (Show, Typeable)

instance Illuminant C n where
    primaries _ = Chromaticity 0.31006 0.31616

data D40 = D40
         deriving (Show, Typeable)

instance Illuminant D40 n where
    primaries _ = Chromaticity 0.3823 0.3838

data D45 = D45
         deriving (Show, Typeable)

instance Illuminant D45 n where
    primaries _ = Chromaticity 0.3621 0.3709

data D50 = D50
         deriving (Show, Typeable)

instance Illuminant D50 n where
    primaries _ = Chromaticity 0.34567 0.35850

data D55 = D55
         deriving (Show, Typeable)

instance Illuminant D55 n where
    primaries _ = Chromaticity 0.33242 0.34743

data D60 = D60
         deriving (Show, Typeable)

instance Illuminant D60 n where
    primaries _ = Chromaticity 0.3217 0.3378

data D65 = D65
         deriving (Show, Typeable)

instance Illuminant D65 n where
    primaries _ = Chromaticity 0.31271 0.32902

data D70 = D70
         deriving (Show, Typeable)

instance Illuminant D70 n where
    primaries _ = Chromaticity 0.3054 0.3216

data D75 = D75
         deriving (Show, Typeable)

instance Illuminant D75 n where
    primaries _ = Chromaticity 0.29902 0.31485

data D93 = D93
         deriving (Show, Typeable)

instance Illuminant D93 n where
    primaries _ = Chromaticity 0.2848 0.2932

data E = E
       deriving (Show, Typeable)

instance Illuminant E n where
    primaries _ = Chromaticity (1/3)  (1/3)

data F1 = F1
        deriving (Show, Typeable)

instance Illuminant F1 n where
    primaries _ = Chromaticity 0.31310 0.33727

data F2 = F2
        deriving (Show, Typeable)

instance Illuminant F2 n where
    primaries _ = Chromaticity 0.37208 0.37529

data F3 = F3
        deriving (Show, Typeable)

instance Illuminant F3 n where
    primaries _ = Chromaticity 0.40910 0.39430

data F4 = F4
        deriving (Show, Typeable)

instance Illuminant F4 n where
    primaries _ = Chromaticity 0.44018 0.40329

data F5 = F5
        deriving (Show, Typeable)

instance Illuminant F5 n where
    primaries _ = Chromaticity 0.31379 0.34531

data F6 = F6
        deriving (Show, Typeable)

instance Illuminant F6 n where
    primaries _ = Chromaticity 0.37790 0.38835

data F7 = F7
        deriving (Show, Typeable)

instance Illuminant F7 n where
    primaries _ = Chromaticity 0.31292 0.32933

data F8 = F8
        deriving (Show, Typeable)

instance Illuminant F8 n where
    primaries _ = Chromaticity 0.34588 0.35875

data F9 = F9
        deriving (Show, Typeable)

instance Illuminant F9 n where
    primaries _ = Chromaticity 0.37417 0.37281

data F10 = F10
        deriving (Show, Typeable)

instance Illuminant F10 n where
    primaries _ = Chromaticity 0.34609 0.35986

data F11 = F11
         deriving (Show, Typeable)

instance Illuminant F11 n where
    primaries _ = Chromaticity 0.38052 0.37713

data F12 = F12
        deriving (Show, Typeable)

instance Illuminant F12 n where
    primaries _ = Chromaticity 0.43695 0.40441

data ACES = ACES
          deriving (Show, Typeable)

instance Illuminant ACES n where
    primaries _ = Chromaticity 0.32168 0.33767

-- correlated color temperature
newtype CCT a = CCT a
              deriving (Show, Typeable)

instance (Num n, Floating n) => Illuminant (CCT n) n where
    primaries (CCT temperature) = Chromaticity x y
        where x = 3 * u / (2 * u - 8 * v + 4)
              y = 2 * v / (2 * u - 8 * v + 4)

              u = (0.860117757 + 1.54118254e-4 * temperature + 1.28641212e-7 * temperature ** 2)
                  /
                  (1 + 9.42420235e-4 * temperature + 7.08145163e-7 * temperature ** 2)

              v = (0.317398726 + 4.22806245e-5 * temperature + 4.20481691e-8 * temperature ** 2)
                  /
                  (1 - 2.89741816e-5 * temperature + 1.61456053e-7 * temperature ** 2)
