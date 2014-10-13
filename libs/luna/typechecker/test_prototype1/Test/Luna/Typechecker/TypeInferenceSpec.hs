module Test.Luna.Typechecker.TypeInferenceSpec (spec) where


import Luna.Typechecker.TypeInference
import Luna.Typechecker.TIMonad         (startTI)
import Luna.Typechecker.Typeclasses

import Luna.Typechecker.AST.ClassID
import Luna.Typechecker.AST.Kind
import Luna.Typechecker.AST.TID
import Luna.Typechecker.AST.Type

import Luna.Typechecker.Internal.Logger

import Test.Hspec
import Test.QuickCheck

import Test.Luna.Typechecker.AST.TypeGen


spec :: Spec
spec = do
  let diamond_classes = addClass (ClassID "ClassA") []
                    <:> addClass (ClassID "ClassB1") [ClassID "ClassA"]
                    <:> addClass (ClassID "ClassB2") [ClassID "ClassA"]
                    <:> addClass (ClassID "ClassC") [ClassID "ClassB1", ClassID "ClassB2"]
      diamond_inst    = addInst [] (IsIn (ClassID "ClassB2") tInt)
                    <:> addInst [] (IsIn (ClassID "ClassA") tInt)
                    <:> addInst [] (IsIn (ClassID "ClassC") tInt)
                    <:> addInst [] (IsIn (ClassID "ClassB1") tInt)
      int_classes     = addClass (ClassID "Integral") []
                    <:> addInst [] (IsIn (ClassID "Integral") tInt)
                    <:> addInst [] (IsIn (ClassID "Integral") tInteger)
      double_classes  = addClass (ClassID "Fractional") []
                    <:> addInst [] (IsIn (ClassID "Fractional") tDouble)
                    <:> addInst [] (IsIn (ClassID "Fractional") tFloat)
  describe "split" $ do
    it "works for single predicate: retains it" $ do
      let type01  = Tyvar (TID "a") Star
          Right ce = evalLogger $ (diamond_classes <:> diamond_inst) initialEnv
          fixvars = []
          toquant = [type01]
          predics = [IsIn (ClassID "ClassA") tInt, IsIn (ClassID "ClassC") tInt, IsIn (ClassID "ClassC") (TVar type01)]
          (Right (deferred, retained), _) = startTI $ runLoggerT (split ce fixvars toquant predics)
      retained `shouldContain` [IsIn (ClassID "ClassC") (TVar type01)]
      deferred `shouldBe` []
    it "works for single predicate: defers it" $ do
      let type01  = Tyvar (TID "a") Star
          Right ce = evalLogger $ (diamond_classes <:> diamond_inst) initialEnv
          fixvars = [type01]
          toquant = []
          predics = [IsIn (ClassID "ClassA") tInt, IsIn (ClassID "ClassC") tInt, IsIn (ClassID "ClassC") (TVar type01)]
          (Right (deferred, retained), _) = startTI $ runLoggerT (split ce fixvars toquant predics)
      deferred `shouldContain` [IsIn (ClassID "ClassC") (TVar type01)]
      retained `shouldBe` []
    it "works for single predicate: defaulting" $ do
      let type01  = Tyvar (TID "a") Star
          type02  = Tyvar (TID "b") Star
          Right ce = evalLogger $ (int_classes <:> double_classes) initialEnv
          fixvars = []
          toquant = []
          predics = [IsIn (ClassID "Integral") (TVar type01), IsIn (ClassID "Fractional") (TVar type02)]
          (Right (deferred, retained), _) = startTI $ runLoggerT (split ce fixvars toquant predics)
      deferred `shouldBe` []
      retained `shouldBe` []
    it "prop: returns [] for null predicate list" $
      property $
      forAll (listOf $ genTyvar Star) $ \fs ->
      forAll (listOf $ genTyvar Star) $ \gs ->
        let (Right (ds, rs), _) = startTI $ runLoggerT (split initialEnv fs gs [])
            in do ds `shouldBe` []
                  rs `shouldBe` []
