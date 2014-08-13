---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------

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

module Luna.Target.HS.Control.Error.ErrorSpec where

import Control.Applicative  
import Control.Monad.IO.Class
import Control.Monad.Trans
import Data.Typeable
import Control.PolyApplicative

import Flowbox.Test.QuickCheck

import Luna.Target.HS.Control.Error
import Luna.Target.HS.Control.Flow
import Flowbox.Utils
import Prelude


------------------------------------------------------------------------
-- Debug Data Types
------------------------------------------------------------------------

data E1 = E1 deriving (Show, Typeable, Eq)
data E2 = E2 deriving (Show, Typeable, Eq)
data E3 = E3 deriving (Show, Typeable, Eq)
data E4 = E4 deriving (Show, Typeable, Eq)
data E5 = E5 deriving (Show, Typeable, Eq)


----------------------------------------------------------------------------

summe :: Int -> Int -> Int
summe = (+)

summe' = liftErr2 summe



main = hspec spec

spec = do
    describe "Touching values" $ do
        it "safe touch"             $ touchErr E1 v    `shouldBe` (UnsafeValue 1 :: UnsafeBase Safe E1 Int)
        it "simple unsafe touch"    $ touchErr E1 e12  `shouldBe` (Error E1 :: UnsafeBase (UnsafeBase Safe E2) E1 Int)
        it "complex unsafe touch 1" $ touchErr E1 e21  `shouldBe` (Error E2 :: UnsafeBase (UnsafeBase Safe E1) E2 Int)
        it "complex unsafe touch 2" $ touchErr E1 e23  `shouldBe` (Error E2 :: UnsafeBase (UnsafeBase (UnsafeBase Safe E1) E3) E2 Int)

    describe "Raising errors" $ do
        it "e1"          $ raise E1 v                                    `shouldBe` (Error E1 :: UnsafeBase Safe E1 Int)
        it "e1 e2"       $ (raise E2 . raise E1) v                       `shouldBe` (Error E1 :: UnsafeBase (UnsafeBase Safe E2) E1 Int)
        it "e1 e2 e1"    $ (raise E1 . raise E2 . raise E1) v            `shouldBe` (Error E1 :: UnsafeBase (UnsafeBase Safe E2) E1 Int)
        it "e3 e1 e2 e1" $ (raise E3 . raise E1 . raise E2 . raise E1) v `shouldBe` (Error E1 :: UnsafeBase (UnsafeBase (UnsafeBase Safe E3) E2) E1 Int)
  
    describe "LiftErr testing" $ do
        describe "[instance] LiftErr Safe Safe Safe" $ do
            it "sum v v"     $ summe' v v     `shouldBe` (Safe (2::Int))
        
        describe "[instance] LiftErr Safe (UnsafeBase base e) (UnsafeBase base e)" $ do
            it "sum v e1"    $ summe' v e1   `shouldBe`  (Error E1 :: UnsafeBase Safe E1 Int)
            it "sum v e123"  $ summe' v e123 `shouldBe`  (Error E1 :: UnsafeBase (UnsafeBase (UnsafeBase Safe E3) E2) E1 Int)
            it "sum v v1x1"  $ summe' v v1x1 `shouldBe`  (UnsafeValue 2  :: UnsafeBase Safe E1 Int)
            it "sum v v2x12" $ summe' v v2x12 `shouldBe` (UnsafeOther (UnsafeValue 2)  :: UnsafeBase (UnsafeBase Safe E2) E1 Int)
            it "sum v v123"  $ summe' v v123 `shouldBe`  (UnsafeValue 2  :: UnsafeBase (UnsafeBase (UnsafeBase Safe E3) E2) E1 Int)
        
        describe "[instance] LiftErr (UnsafeBase base e) Safe (UnsafeBase base e)" $ do
            it "sum e1   v"  $ summe' e1    v `shouldBe` (Error E1 :: UnsafeBase Safe E1 Int)
            it "sum e123 v"  $ summe' e123  v `shouldBe` (Error E1 :: UnsafeBase (UnsafeBase (UnsafeBase Safe E3) E2) E1 Int)
            it "sum v1x1 v"  $ summe' v1x1  v `shouldBe` (UnsafeValue 2  :: UnsafeBase Safe E1 Int)
            it "sum v2x12 v" $ summe' v2x12 v `shouldBe` (UnsafeOther (UnsafeValue 2)  :: UnsafeBase (UnsafeBase Safe E2) E1 Int)
            it "sum v123 v"  $ summe' v123  v `shouldBe` (UnsafeValue 2  :: UnsafeBase (UnsafeBase (UnsafeBase Safe E3) E2) E1 Int)
        
        describe "[~] LiftErr (UnsafeBase base e) (UnsafeBase base e) (UnsafeBase base e)" $ do
            it "sum e1 e1"     $ summe' e1 e1     `shouldBe` (Error E1 :: UnsafeBase Safe E1 Int)
            it "sum e123 e123" $ summe' e123 e123 `shouldBe` (Error E1 :: UnsafeBase (UnsafeBase (UnsafeBase Safe E3) E2) E1 Int)
            it "sum v1x1 v1x1" $ summe' v1x1 v1x1 `shouldBe` (UnsafeValue 2  :: UnsafeBase Safe E1 Int)
        
        describe "[instance] LiftErr (UnsafeBase base1 e) (UnsafeBase base2 e) (UnsafeBase dstBase e)" $ do
            it "sum e1 e1"       $ summe' e1 e1       `shouldBe` (Error E1 :: UnsafeBase Safe E1 Int)
            it "sum e34 e3"      $ summe' e34 e3      `shouldBe` (Error E3 :: UnsafeBase (UnsafeBase Safe E4) E3 Int)
            it "sum e3 e34"      $ summe' e3 e34      `shouldBe` (Error E3 :: UnsafeBase (UnsafeBase Safe E4) E3 Int)
            it "sum e34 e35"     $ summe' e34 e35     `shouldBe` (Error E3 :: UnsafeBase (UnsafeBase (UnsafeBase Safe E5) E4) E3 Int)
            it "sum v2x12 v3x13" $ summe' v2x12 v3x13 `shouldBe` (UnsafeOther (UnsafeValue 2) :: UnsafeBase (UnsafeBase (UnsafeBase Safe E3) E2) E1 Int)
    
        describe "[instance] LiftErr (UnsafeBase base1 e1) (UnsafeBase base2 e2) (UnsafeBase dstBase e1)" $ do
            it "sum e23 e34"     $ summe' e23 e34     `shouldBe` (Error E2 :: UnsafeBase (UnsafeBase (UnsafeBase Safe E4) E3) E2 Int)
            it "sum v2x12 v1x21" $ summe' v2x12 v1x21 `shouldBe` (UnsafeOther (UnsafeValue 2) :: UnsafeBase (UnsafeBase Safe E2) E1 Int)
            it "sum v2x12 v4x34" $ summe' v2x12 v4x34 `shouldBe` (UnsafeOther (UnsafeValue 2) :: UnsafeBase (UnsafeBase (UnsafeBase (UnsafeBase Safe E4) E3) E2) E1 Int)
            it "sum v12 v4x1234" $ summe' v12 v4x1234 `shouldBe` (UnsafeOther . UnsafeOther . UnsafeOther $ (UnsafeValue 2) :: UnsafeBase (UnsafeBase (UnsafeBase (UnsafeBase Safe E4) E3) E2) E1 Int)

    describe "Catching errors" $ do
        describe "Basic catching" $ do
            it "Safe Unsafevalue catching"        $ catch (\E1 -> Safe(0::Int)) (Safe (1::Int)) `shouldBe` (Safe 1 :: Safe Int)
            it "Single-error Unsafevalue catching"      $ catch (\E1 -> Safe(0::Int)) (raise E1 $ Safe (1::Int)) `shouldBe` (Safe 0 :: Safe Int)
            it "Single-error Unsafevalue miss-catching" $ catch (\E1 -> Safe(0::Int)) (raise E2 $ Safe (1::Int)) `shouldBe` (Error E2 :: UnsafeBase Safe E2 Int)
            it "Multi-error Unsafevalue catching 1"     $ catch (\E1 -> Safe(0::Int)) (raise E2 $ raise E1 $ Safe (1::Int)) `shouldBe` (UnsafeValue 0 :: UnsafeBase Safe E2 Int)
            it "Multi-error Unsafevalue catching 2"     $ catch (\E2 -> Safe(0::Int)) (raise E2 $ raise E1 $ Safe (1::Int)) `shouldBe` (Error E1 :: UnsafeBase Safe E1 Int)
            it "Multi-error Unsafevalue miss-catching"  $ catch (\E4 -> Safe(0::Int)) (raise E2 $ raise E1 $ Safe (1::Int)) `shouldBe` (Error E1 :: UnsafeBase (UnsafeBase Safe E2) E1 Int)
            it "Double catching 1"                $ (catch (\E2 -> Safe(2::Int)) $ catch (\E1 -> Safe(1::Int)) (raise E2 $ raise E1 $ Safe (1::Int))) `shouldBe` (Safe 1 :: Safe Int)
            it "Double catching 2"                $ (catch (\E1 -> Safe(1::Int)) $ catch (\E2 -> Safe(2::Int)) (raise E2 $ raise E1 $ Safe (1::Int))) `shouldBe` (Safe 1 :: Safe Int)
            it "Complex catching"                 $ (catch (\E2 -> Safe(2::Int)) $ catch (\E3 -> Safe(3::Int)) $ catch (\E2 -> Safe(2::Int)) $ catch (\E1 -> Safe(1::Int)) (raise E2 $ raise E3 $ Safe (1::Int))) `shouldBe` (Safe 3 :: Safe Int)
        describe "Re-raising errors" $ do
            it "Safe Unsafevalue re-raising"              $ catch (\E1 -> ReRaise) (Safe (1::Int)) `shouldBe` (Safe 1 :: Safe Int)
            it "Signle-error Unsafevalue re-raising"      $ catch (\E2 -> ReRaise) (raise E2 $ Safe (1::Int)) `shouldBe` (Error E2 :: UnsafeBase Safe E2 Int)
            it "Signle-error Unsafevalue miss-re-raising" $ catch (\E1 -> ReRaise) (raise E2 $ Safe (1::Int)) `shouldBe` (Error E2 :: UnsafeBase Safe E2 Int)
        describe "Nested error raising" $ do
            it "Safe Unsafevalue nested raising"      $ catch (\E1 -> raise E1 $ Safe(0::Int)) (Safe (1::Int)) `shouldBe` (Safe 1 :: Safe Int)
            it "Monotype monobase nested raising"     $ catch (\E1 -> raise E1 $ Safe(0::Int)) (raise E1 $ Safe (1::Int)) `shouldBe` (Error E1 :: UnsafeBase Safe E1 Int)
            it "Politype monobase nested raising"     $ catch (\E1 -> raise E2 $ Safe(0::Int)) (raise E1 $ Safe (1::Int)) `shouldBe` (Error E2 :: UnsafeBase Safe E2 Int)
            it "Politype polibase nested raising"     $ catch (\E1 -> e23) (e1) `shouldBe` (Error E2 :: UnsafeBase (UnsafeBase Safe E3) E2 Int)
            --FIXME: it "Politype polibase nested mis-raising" $ catch (\E1 -> e23) (e2) `shouldBe` (Error E2 :: UnsafeBase (UnsafeBase Safe E3) E2 Int)


v  = Safe(1::Int)

e1    = raise E1 v    :: UnsafeBase Safe E1 Int
e2    = raise E2 v    :: UnsafeBase Safe E2 Int
e3    = raise E3 v    :: UnsafeBase Safe E3 Int
e4    = raise E4 v    :: UnsafeBase Safe E4 Int
e12   = raise E2 e1   :: UnsafeBase (UnsafeBase Safe E2) E1 Int
e13   = raise E3 e1   :: UnsafeBase (UnsafeBase Safe E3) E1 Int
e21   = raise E1 e2   :: UnsafeBase (UnsafeBase Safe E1) E2 Int
e23   = raise E3 e2   :: UnsafeBase (UnsafeBase Safe E3) E2 Int
e34   = raise E4 e3   :: UnsafeBase (UnsafeBase Safe E4) E3 Int
e35   = raise E5 e3   :: UnsafeBase (UnsafeBase Safe E5) E3 Int
e123  = raise E3 e12  :: UnsafeBase (UnsafeBase (UnsafeBase Safe E3) E2) E1 Int
e1234 = raise E4 e123 :: UnsafeBase (UnsafeBase (UnsafeBase (UnsafeBase Safe E4) E3) E2) E1 Int


v12 = UnsafeValue 1 :: UnsafeBase (UnsafeBase Safe E2) E1 Int
v123 = UnsafeValue 1 :: UnsafeBase (UnsafeBase (UnsafeBase Safe E3) E2) E1 Int
v1234 = UnsafeValue 1 :: UnsafeBase (UnsafeBase (UnsafeBase (UnsafeBase Safe E4) E3) E2) E1 Int
--vx1 = UnsafeValue 1 :: UnsafeBase (UnsafeBase (UnsafeBase (UnsafeBase Safe E4) E3) E2) E1 Int
--vx2 = UnsafeOther $ UnsafeValue 1 :: UnsafeBase (UnsafeBase (UnsafeBase (UnsafeBase Safe E4) E3) E2) E1 Int
--vx3 = UnsafeOther . UnsafeOther $ UnsafeValue 1 :: UnsafeBase (UnsafeBase (UnsafeBase (UnsafeBase Safe E4) E3) E2) E1 Int
--vx4 = UnsafeOther . UnsafeOther . UnsafeOther $ UnsafeValue 1 :: UnsafeBase (UnsafeBase (UnsafeBase (UnsafeBase Safe E4) E3) E2) E1 Int

v1x1 = UnsafeValue 1 :: UnsafeBase Safe E1 Int
v2x2 = UnsafeValue 1 :: UnsafeBase Safe E2 Int
v3x3 = UnsafeValue 1 :: UnsafeBase Safe E3 Int
v4x4 = UnsafeValue 1 :: UnsafeBase Safe E4 Int
v2x12 = UnsafeOther $ UnsafeValue 1 :: UnsafeBase (UnsafeBase Safe E2) E1 Int
v4x34 = UnsafeOther $ UnsafeValue 1 :: UnsafeBase (UnsafeBase Safe E4) E3 Int

v3x13 = UnsafeOther $ UnsafeValue 1 :: UnsafeBase (UnsafeBase Safe E3) E1 Int

v1x21 = UnsafeOther $ UnsafeValue 1 :: UnsafeBase (UnsafeBase Safe E1) E2 Int
v1x31 = UnsafeOther $ UnsafeValue 1 :: UnsafeBase (UnsafeBase Safe E1) E3 Int

v1x1234 = UnsafeValue 1 :: UnsafeBase (UnsafeBase (UnsafeBase (UnsafeBase Safe E4) E3 ) E2) E1 Int
v2x1234 = UnsafeOther (UnsafeValue 1) :: UnsafeBase (UnsafeBase (UnsafeBase (UnsafeBase Safe E4) E3 ) E2) E1 Int
v3x1234 = UnsafeOther . UnsafeOther $ (UnsafeValue 1) :: UnsafeBase (UnsafeBase (UnsafeBase (UnsafeBase Safe E4) E3 ) E2) E1 Int
v4x1234 = UnsafeOther . UnsafeOther . UnsafeOther $ (UnsafeValue 1) :: UnsafeBase (UnsafeBase (UnsafeBase (UnsafeBase Safe E4) E3 ) E2) E1 Int
