---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverlappingInstances  #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}

module Flowbox.Graphics.Color.Companding (
      Companding(..)
    , AlexaV3LogC(..)
    , Cineon(..)
    , Gamma(..)
    , Linear(..)
    , LStar(..)
    , Panalog(..)
    , PLogLin(..)
    , Rec709(..)
    , REDLog(..)
    , SLog(..)
    , SRGBGamma(..)
    , ViperLog(..)
    ) where

import qualified Data.Array.Accelerate as A

import Flowbox.Graphics.Utils.Accelerate (variable)
import Flowbox.Prelude



class (Num b, Floating b) => Companding a b where
    toLinear   :: a -> b -> b
    fromLinear :: a -> b -> b


data AlexaV3LogC = AlexaV3LogC
                 deriving Show

instance (Num a, Floating a, a ~ A.Exp t, A.Elt t, A.IsFloating t) => Companding AlexaV3LogC a where
    toLinear   _ v = v A.>* eCutF A.? ((10 ** ((v - alexad) / alexac) - alexab) / alexaa, (v - alexaf) / alexae)

    fromLinear _ v = v A.>* cut A.? (alexac * logBase 10 (alexaa * v + alexab) + alexad, alexae * v + alexaf)

alexaa, alexab, alexac, alexad, alexae, alexaf, cut, eCutF :: (A.Elt a, A.IsFloating a) => A.Exp a
alexaa     = 5.555556
alexab     = 0.052272
alexac     = 0.247190
alexad     = 0.385537
alexae     = 5.367655
alexaf     = 0.092809
cut   = 0.010591
eCutF = alexae * cut + alexaf


data Cineon = Cineon
            deriving Show

instance (Num a, Floating a, a ~ A.Exp t, A.Elt t, A.IsFloating t) => Companding Cineon a where
    toLinear   _ v = (10 ** ((1023 * v - 685) / 300) - cineonBlackOffset) / (1 - cineonBlackOffset)

    fromLinear _ v = (685 + 300 * logBase 10 (v * (1 - cineonBlackOffset) + cineonBlackOffset)) / 1023

cineonBlackOffset :: (A.Elt e, A.IsFloating e) => A.Exp e
cineonBlackOffset = 10 ** ((95 - 685) / 300)


data Gamma a = Gamma a
             deriving Show

instance (a ~ A.Plain a, A.IsFloating a, A.Elt a, A.Lift A.Exp a) => Companding (Gamma a) (A.Exp a) where
    toLinear   (Gamma g) v = v ** variable g

    fromLinear (Gamma g) v = v ** (1 / variable g)

instance (A.IsFloating t, A.Elt t) => Companding (Gamma (A.Exp t)) (A.Exp t) where
    toLinear (Gamma g) v = v ** g

    fromLinear (Gamma g) v = v ** (1 / g)


data Linear = Linear
            deriving Show

instance Floating a => Companding Linear a where
    toLinear   _ v = v
    fromLinear _ v = v


data LStar = LStar
           deriving Show

instance (Num a, a ~ A.Exp t, A.Elt t, A.IsFloating t) => Companding LStar a where
    toLinear   _ v = v A.<=* 0.08 A.? (100 * v / lstark, ((v + 0.16) / 1.16) ** 3)

    fromLinear _ v = v A.<=* lstare A.? (v * lstark / 100, 1.16 * v ** (1/3) - 0.16)

lstark :: (A.Elt e, A.IsFloating e) => A.Exp e
lstark = 903.3 -- actual CIE standard
 -- 24389/27 - intent of the CIE standard

lstare :: (A.Elt e, A.IsFloating e) => A.Exp e
lstare = 0.008856 -- actual CIE standard
 -- 216/24389 - intent of the CIE standard


data Panalog = Panalog
             deriving Show

instance (Num a, Floating a, a ~ A.Exp t, A.Elt t, A.IsFloating t) => Companding Panalog a where
    toLinear   _ v = (10 ** ((1023 * v - 681) / 444) - panalogBlackOffset) / (1 - panalogBlackOffset)

    fromLinear _ v = (681 + 444 * logBase 10 (v * (1 - panalogBlackOffset) + panalogBlackOffset)) / 1023

panalogBlackOffset :: (A.Elt e, A.IsFloating e) => A.Exp e
panalogBlackOffset = 10 ** ((64 - 681) / 444)


data PLogLin = PLogLin
             deriving Show

instance (Num a, Floating a, a ~ A.Exp t, A.Elt t, A.IsFloating t) => Companding PLogLin a where
    toLinear   _ v = 10 ** ((v * 1023 - logReference) * dpcvOverNg) * linReference

    fromLinear _ v = ((logBase 10 (linReference * v) / dpcvOverNg) + logReference) / 1023

logReference,  linReference, dpcvOverNg, densityPerCodeValue, negativeGamma :: (A.Elt e, A.IsFloating e) => A.Exp e
logReference = 445
linReference = 0.18
dpcvOverNg   = densityPerCodeValue / negativeGamma
densityPerCodeValue = 0.002
negativeGamma = 0.6


data Rec709 = Rec709
            deriving Show

instance (Num a, a ~ A.Exp t, A.Elt t, A.IsFloating t) => Companding Rec709 a where
    toLinear   _ v = v A.<=* 0.081 A.? (v / 4.5, ((v + 0.099) / 1.099) ** (1 / 0.45))

    fromLinear _ v = v A.<=* 0.018 A.? (v * 4.5, (1.099 * v ** 0.45) - 0.099)


data REDLog = REDLog
            deriving Show

instance (Num a, Floating a, a ~ A.Exp t, A.Elt t, A.IsFloating t) => Companding REDLog a where
    toLinear   _ v = ((10 ** ((1023 * v - 1023) / 511)) - redBlackOffset) / (1 - redBlackOffset)

    fromLinear _ v = (1023 + 511 * logBase 10 (v * (1 - redBlackOffset) + redBlackOffset)) / 1023

redBlackOffset :: (A.Elt e, A.IsFloating e) => A.Exp e
redBlackOffset = 10 ** (negate 1023 / 511)


data SLog = SLog
          deriving Show

instance (Num a, Floating a, a ~ A.Exp t, A.Elt t, A.IsFloating t) => Companding SLog a where
    toLinear   _ v = 10 ** ((v - sloga - slogb) / slogc) - slogd

    fromLinear _ v = slogc * logBase 10 (v + slogd) + slogb + sloga

sloga, slogb, slogc, slogd :: (A.Elt e, A.IsFloating e) => A.Exp e
sloga = 0.616596
slogb = 0.03
slogc = 0.432699
slogd = 0.037584


data ViperLog = ViperLog
              deriving Show

instance (Num a, Floating a) => Companding ViperLog a where
    toLinear   _ v = 10 ** ((1023 * v - 1023) / 500)

    fromLinear _ v = (500 * logBase 10 v + 1023) / 1023


data SRGBGamma = SRGBGamma
               deriving Show

instance (Num a, Floating a, a ~ A.Exp t, A.Elt t, A.IsScalar t) => Companding SRGBGamma a where
    toLinear   _ v = v A.<=* 0.04045 A.? (v / 12.92, ((v + 0.055) / 1.055) ** 2.4)

    fromLinear _ v = v A.<=* 0.0031308 A.? (12.92 * v, (1.055 * v) ** (1 / 2.4) - 0.055)
