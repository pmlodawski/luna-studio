---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------

{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveDataTypeable #-}

{-# LANGUAGE OverlappingInstances #-}

{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE CPP #-}


!{-# LANGUAGE RightSideContexts #-}
!{-# LANGUAGE Python #-}

module Test.Luna.Comp.SamplesSpec where

import Prelude
import Flowbox.Test.QuickCheck
import Flowbox.Utils


------------------------------------------------------------------------
-- Debug Data Types
------------------------------------------------------------------------


----------------------------------------------------------------------------

main = hspec spec

spec = do
    describe "Touching values" $ do
        it "safe touch"             $ (1::Int)    `shouldBe` (1::Int)
