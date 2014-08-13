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

module Luna.Target.HS.Control.Context.ContextSpec where

import Flowbox.Test.QuickCheck
import Luna.Target.HS.Control.Context
import Luna.Target.HS.Utils.BaseMonads
import System.IO.Unsafe
import Prelude

instance Eq a => Eq (IO a) where
    a == b = unsafePerformIO a == unsafePerformIO b 
    a /= b = unsafePerformIO a /= unsafePerformIO b 

main = hspec spec

spec = do
        describe "Running monads" $ do
            it "running single monad"          $ runStateTX getX   0                         `shouldBeStrict` (Pure (0,0))
            it "monad mono-concatenation"      $ (flip runStateTX  0 $ getX `bindEnv_` getX) `shouldBeStrict` (Pure (0,0))
            it "monad poly-concatenation"      $ (flip runReaderTX 5 $ flip runStateTX  0 $ getX `bindEnv_` askX)      `shouldBeStrict` (Pure (5,0))
            it "monad poly-concatenation flip" $ (flip runStateTX  0 $ flip runReaderTX 5 $ getX `bindEnv_` askX)      `shouldBeStrict` (Pure (5,0))
            it "monad poly-concatenation flip" $ (flip runStateTX  0 $ flip runReaderTX 5 $ getX `bindEnv_` askX)      `shouldBeStrict` (Pure (5,0))
            it "Pure value monad post-multi binding" $ (flip runReaderTX 5 $ flip runStateTX  0 $ getX `bindEnv_` askX `bindEnv_` (returnPure 1)) `shouldBeStrict` (Pure (1,0))
            it "Pure value monad pre-multi binding"  $ (flip runReaderTX 5 $ flip runStateTX  0 $ (returnPure 1) `bindEnv_` getX `bindEnv_` askX) `shouldBeStrict` (returnPure (5,0))
            it "IO value monad post-multi binding"   $ (flip runReaderTX 5 $ flip runStateTX  0 $ getX `bindEnv_` askX `bindEnv_` (returnIO 1))   `shouldBeStrict` (returnIO (1::Int,0::Int))
            it "IO value monad pre-single binding" $ (flip runStateTX  0 $ (return 1 :: Num a => IO a) `bindEnv_` getX)   `shouldBeStrict` (returnIO (0::Int,0::Int))
            it "IO value monad pre-multi binding"  $ (flip runStateTX  0 $ (return 1 :: Num a => IO a) `bindEnv_` getX)   `shouldBeStrict` (returnIO (0::Int,0::Int))
            
    putStrLn "Other tests:"
    print =<< (flip runStateTX 0 $ flip runReaderTX 0 $ getX `bindEnv_` askX `bindEnv_` (return 1 :: Num a => IO a))
    print $ flip runReaderTX 5 $ flip runStateTX 0 $ (return 1 :: Num a => Pure a) `bindEnv_` getX `bindEnv_` askX
    print =<< (flip runStateTX 0 $ (return 1 :: Num a => IO a) `bindEnv_` getX)
    print =<< (flip runStateTX 0 $ flip runReaderTX 0 $ (return 1 :: Num a => IO a) `bindEnv_` getX `bindEnv_` askX)
    putStrLn ""


