---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}

module Flowbox.Graphics.Color.ChromaticAdaptation where

import qualified Data.Array.Accelerate as A
import qualified Data.Distributive     as Distributive
import           Linear ((^*), _x, _y, _z)
import qualified Linear

import Flowbox.Graphics.Color.CIE.XYZ
import Flowbox.Graphics.Color.CIE.XyY
import Flowbox.Graphics.Color.RGB
import Flowbox.Graphics.Color.Gamma
import Flowbox.Graphics.Color.Illuminants as Illuminants
import Flowbox.Graphics.Color.Profile     as Profile
import Flowbox.Graphics.Color.Profile.AlexaWideGamutRGB
import Flowbox.Prelude


--vonKries :: A.Array A.DIM2 Float
--vonKries = A.fromList (A.Z A.:. 3 A.:. 3) [
--    0.40024,   0.7076,  (-0.08081),
--    (-0.2263), 1.16532, 0.0457,
--    0,         0,       0.91822
--    ]
vonKries :: (Num a, Floating a) => Linear.M33 a
vonKries = Linear.V3 (Linear.V3   0.40024 0.7076  (-0.08081))
                     (Linear.V3 (-0.2263) 1.16532   0.0457)
                     (Linear.V3   0       0         0.91822)

--vonKriesInverse :: A.Array A.DIM2 Float
--vonKriesInverse = A.fromList (A.Z A.:. 3 A.:. 3) [
--    1.8599364, (-1.1293816), 0.2198974,
--    0.3611914, 0.6388125   , (-0.0000064),
--    0        , 0           , 1.0890636
--    ]
vonKriesInverse :: (Num a, Floating a) => Linear.M33 a
vonKriesInverse = Linear.V3 (Linear.V3 1.8599364 (-1.1293816)   0.2198974)
                            (Linear.V3 0.3611914   0.6388125  (-0.0000064))
                            (Linear.V3 0           0            1.0890636)

--bradford :: A.Array A.DIM2 Float
--bradford = A.fromList (A.Z A.:. 3 A.:. 3) [
--    0.8951,     0.2664,    (-0.1614),
--    (-0.7502),  1.7135,    0.0367,
--    0.0389,     (-0.0685), 1.0296
--    ]
bradford :: (Num a, Floating a) => Linear.M33 a
bradford = Linear.V3 (Linear.V3   0.8951    0.2664  (-0.1614))
                     (Linear.V3 (-0.7502)   1.7135    0.0367)
                     (Linear.V3   0.0389  (-0.0685)   1.0296)

--bradfordInverse :: A.Array A.DIM2 Float
--bradfordInverse = A.fromList (A.Z A.:. 3 A.:. 3) [
--    0.9869929,    (-0.1470543), 0.1599627,
--    0.4323053,    0.5183603,    0.0492912,
--    (-0.0085287), 0.0400428,    0.9684867
--    ]
bradfordInverse :: (Num a, Floating a) => Linear.M33 a
bradfordInverse = Linear.V3 (Linear.V3   0.9869929  (-0.1470543) 0.1599627)
                            (Linear.V3   0.4323053    0.5183603  0.0492912)
                            (Linear.V3 (-0.0085287)   0.0400428  0.9684867)

xyzScaling :: (Num a, Floating a) => Linear.M33 a
xyzScaling = Linear.eye3

xyzScalingInverse :: (Num a, Floating a) => Linear.M33 a
xyzScalingInverse = xyzScaling

data AdaptationMethod = XYZScaling
                      | Bradford
                      | VonKries
                      deriving (Show)

adaptationMatrix :: (Num a, Floating a) => AdaptationMethod -> Linear.M33 a
adaptationMatrix XYZScaling = xyzScaling
adaptationMatrix Bradford   = bradford
adaptationMatrix VonKries   = vonKries

inverseAdaptationMatrix :: (Num a, Floating a) => AdaptationMethod -> Linear.M33 a
inverseAdaptationMatrix XYZScaling = xyzScalingInverse
inverseAdaptationMatrix Bradford   = bradfordInverse
inverseAdaptationMatrix VonKries   = vonKriesInverse

chromaticAdaptation :: forall a b c. (Eq c, Num c, Floating c, Illuminant a c, Illuminant b c)
                    => AdaptationMethod -> a -> b -> XYZ c -> XYZ c
chromaticAdaptation adaptationMethod sourceWhitepoint destinationWhitepoint xyz = unV3 $ m Linear.!* toV3 xyz
    where m = chromaticAdaptationMatrix adaptationMethod sourceWhitepoint destinationWhitepoint

chromaticAdaptationMatrix :: forall a b c. (Eq c, Num c, Floating c, Illuminant a c, Illuminant b c)
                          => AdaptationMethod -> a -> b -> Linear.M33 c
chromaticAdaptationMatrix adaptationMethod sourceWhitepoint destinationWhitepoint = m
    where m = (inverseAdaptationMatrix adaptationMethod Linear.!*! coneResponseMatrix) Linear.!*! adaptationMatrix adaptationMethod
          coneResponseMatrix        = diag rho gamma beta
              where Linear.V3 rho gamma beta = destinationResponseVector / sourceResponseVector
          sourceResponseVector      = adaptationMatrix adaptationMethod Linear.!* srcWhiteVec
          destinationResponseVector = adaptationMatrix adaptationMethod Linear.!* dstWhiteVec

          srcWhiteVec = toV3 . Illuminants.toXYZ' . toxyY . Illuminants.primaries $ sourceWhitepoint
          dstWhiteVec = toV3 . Illuminants.toXYZ' . toxyY . Illuminants.primaries $ destinationWhitepoint

          diag a b c = Linear.V3 (Linear.V3 a 0 0) (Linear.V3 0 b 0) (Linear.V3 0 0 c)

toV3 (XYZ x y z) = Linear.V3 x y z
unV3 (Linear.V3 x y z) = XYZ x y z

-- FIXME[MM]: assumes that the numbers are plain Haskell, Linear can't invert 3x3 on GPU
rgb2XYZMatrix :: forall a b c d t. (RGBProfile a b, Illuminant d b, Num b, Linear.Epsilon b, Floating b, Eq b)
              => a c b -> d -> Maybe (Linear.M33 b)
rgb2XYZMatrix rgb source = do
    m <- whitepointAdjusted
    let redColumn   = redPrimary   ^* m ^. _x
        greenColumn = greenPrimary ^* m ^. _y
        blueColumn  = bluePrimary  ^* m ^. _z
    return $ Linear.V3 redColumn greenColumn blueColumn
    where sourceVec :: Linear.V3 b
          sourceVec  = toV3 . Illuminants.toXYZ' . toxyY . Illuminants.primaries $ source

          (r, g, b) = Profile.primaries rgb

          redPrimary, greenPrimary, bluePrimary :: Linear.V3 b
          redPrimary   = toV3 . Illuminants.toXYZ' . toxyY $ r
          greenPrimary = toV3 . Illuminants.toXYZ' . toxyY $ g
          bluePrimary  = toV3 . Illuminants.toXYZ' . toxyY $ b

          primariesMatrix :: Maybe (Linear.M33 b)
          primariesMatrix = Linear.inv33 $ Linear.V3 redPrimary greenPrimary bluePrimary

          whitepointAdjusted :: Maybe (Linear.V3 b)
          whitepointAdjusted = liftA2 (Linear.!*) (Distributive.distribute <$> primariesMatrix) (pure sourceVec)
