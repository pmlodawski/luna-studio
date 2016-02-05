---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Flowbox Team <contact@flowbox.io>, 2014
-- Proprietary and confidential
-- Unauthorized copying of this file, via any medium is strictly prohibited
---------------------------------------------------------------------------
{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# LANGUAGE TemplateHaskell #-}

module Luna.Interpreter.Session.Profile.Info where

import qualified Data.Time.Clock as Clock

import           Flowbox.Prelude



data ProfileInfo = ProfileInfo { _totalCPUTime  :: Double
                               , _totalRealTime :: Clock.NominalDiffTime
                               , _compileTime   :: Clock.NominalDiffTime
                               , _computeTime   :: Clock.NominalDiffTime
                               } deriving (Show, Eq, Ord)

makeLenses ''ProfileInfo

instance Default ProfileInfo where
    def = ProfileInfo def 0 0 0

instance Monoid ProfileInfo where
    mempty = def
    mappend (ProfileInfo a1 a2 a3 a4) (ProfileInfo b1 b2 b3 b4) =
        ProfileInfo (a1 + b1) (a2 + b2) (a3 + b3) (a4 + b4)
