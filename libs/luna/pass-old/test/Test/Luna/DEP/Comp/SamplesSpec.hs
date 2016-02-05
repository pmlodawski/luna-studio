---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------

{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

{-# LANGUAGE DeriveDataTypeable        #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE FunctionalDependencies    #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE TypeOperators             #-}

{-# LANGUAGE OverlappingInstances      #-}

{-# LANGUAGE CPP                       #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE UndecidableInstances      #-}





module Test.Luna.DEP.Comp.SamplesSpec where

import           Flowbox.Test.QuickCheck
import           Prelude


------------------------------------------------------------------------
-- Debug Data Types
------------------------------------------------------------------------


----------------------------------------------------------------------------

main = hspec spec

spec = do
    describe "Touching values" $ do
        it "safe touch"             $ (1::Int)    `shouldBe` (1::Int)
