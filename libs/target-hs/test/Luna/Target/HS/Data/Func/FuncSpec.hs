---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------

{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Luna.Target.HS.Data.Func.FuncSpec where

import Data.Typeable
import Flowbox.Test.QuickCheck
import Luna.Target.HS.Data.Func
import Data.TupleList
import Prelude


data Vector = Vector deriving (Show, Eq, Typeable)

newtype Prop_Vector_length obj = Prop_Vector_length obj deriving (Show, Eq, Typeable)

foo (a,(b,())) = (a,b)
instance Func (Prop_Vector_length Vector) (a,(b,())) (a,b) where
    getFunc _ = foo

handler1    = appH (Prop_Vector_length Vector) $ (mkArg :: NParam "arg1")                           // (mkArg 0 :: DParam Int) // ()
handler1_p1 = appH (Prop_Vector_length Vector) $ (mkArg 1 :: Named "arg1" (Untyped (Provided Int))) // (mkArg 0 :: DParam Int) // ()
handler1_d  = appH (Prop_Vector_length Vector) $ (mkArg 1 :: DParam Int)                            // (mkArg 2 :: DParam Int) // ()


argSet1      = (mkArg :: NParam "arg1")                           // (mkArg :: NParam "arg2")                           // (mkArg 0 :: NDParam "arg3" Int)                    // ()
argSet1_p1   = (mkArg 1 :: Named "arg1" (Untyped (Provided Int))) // (mkArg :: NParam "arg2")                           // (mkArg 0 :: NDParam "arg3" Int)                    // ()
argSet1_p2   = (mkArg :: NParam "arg1")                           // (mkArg 2 :: Named "arg2" (Untyped (Provided Int))) // (mkArg 0 :: NDParam "arg3" Int)                    // ()
argSet1_p12  = (mkArg 1 :: Named "arg1" (Untyped (Provided Int))) // (mkArg 2 :: Named "arg2" (Untyped (Provided Int))) // (mkArg 0 :: NDParam "arg3" Int)                    // ()
argSet1_p123 = (mkArg 1 :: Named "arg1" (Untyped (Provided Int))) // (mkArg 2 :: Named "arg2" (Untyped (Provided Int))) // (mkArg 3 :: Named "arg3" (Untyped (Provided Int))) // ()

----------------------------------------------------------------------------

main = do
    hspec $ do
        describe "Arg application" $ do
            describe "Single argument application" $ do
                it "Unprovided arg" $ appArg (1::Int) (mkArg   :: Unnamed (Untyped Unprovided))     `shouldBe` ( Unnamed (Untyped (Provided 1)) :: (Unnamed (Untyped (Provided Int))) )
                it "Default arg"    $ appArg (1::Int) (mkArg 0 :: Unnamed (Untyped (Default Int)))  `shouldBe` ( Unnamed (Untyped (Provided 1)) :: (Unnamed (Untyped (Provided Int))) )
                it "Provided arg"   $ appArg (1::Int) (mkArg 0 :: Unnamed (Untyped (Provided Int))) `shouldBe` ( Unnamed (Untyped (Provided 1)) :: (Unnamed (Untyped (Provided Int))) )

            describe "Multi argument application" $ do
                it "1 arg app" $ appNextArg (1::Int) argSet1     `shouldBe` argSet1_p1
                it "2 arg app" $ appNextArg (2::Int) argSet1_p1  `shouldBe` argSet1_p12
                it "3 arg app" $ appNextArg (3::Int) argSet1_p12 `shouldBe` argSet1_p123

            describe "Keyword based application" $ do
                it "1st arg app"        $ appArgByName (Proxy::Proxy "arg1") (1::Int) argSet1                                               `shouldBe` argSet1_p1
                it "2st arg app"        $ appArgByName (Proxy::Proxy "arg2") (2::Int) argSet1                                               `shouldBe` argSet1_p2
                it "double arg setting" $ appArgByName (Proxy::Proxy "arg2") (2::Int) (appArgByName (Proxy::Proxy "arg2") (3::Int) argSet1) `shouldBe` argSet1_p2

            describe "Argument reading" $ do
                it "Reading all args"     $ readArgs argSet1_p123 `shouldBe` ((1::Int)//(2::Int)//(3::Int)//())
                it "Reading default args" $ readArgs argSet1_p12  `shouldBe` ((1::Int)//(2::Int)//(0::Int)//())

        describe "Function call" $ do
            it "Default args function call" $ call handler1_d `shouldBe` ((1,2)::(Int,Int))

        describe "Func handler arg application" $ do
            it "Simple arg app" $ appNext (1::Int) handler1 `shouldBe` handler1_p1
            it "Auto call"      $ (curryNext (2::Int) $ curryNext (1::Int) handler1) `shouldBe` ((1,2)::(Int,Int))

    putStrLn ""


