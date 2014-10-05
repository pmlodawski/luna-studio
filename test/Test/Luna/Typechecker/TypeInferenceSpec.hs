module Test.Luna.Typechecker.TypeInferenceSpec (spec) where


import Luna.Typechecker.TypeInference
import Luna.Typechecker.TIMonad         (startTI)
import Luna.Typechecker.Typeclasses

import Luna.Typechecker.AST.Kind
import Luna.Typechecker.AST.TID
import Luna.Typechecker.AST.Type

import Luna.Typechecker.Internal.Logger

import Test.Hspec
import Test.QuickCheck

import Test.Luna.Typechecker.AST.TypeGen


spec :: Spec
spec = do
  let diamond_classes = addClass (TID "ClassA") []
                    <:> addClass (TID "ClassB1") [TID "ClassA"]
                    <:> addClass (TID "ClassB2") [TID "ClassA"]
                    <:> addClass (TID "ClassC") [TID "ClassB1", TID "ClassB2"]
      diamond_inst    = addInst [] (IsIn (TID "ClassB2") tInt)
                    <:> addInst [] (IsIn (TID "ClassA") tInt)
                    <:> addInst [] (IsIn (TID "ClassC") tInt)
                    <:> addInst [] (IsIn (TID "ClassB1") tInt)
      int_classes     = addClass (TID "Integral") []
                    <:> addInst [] (IsIn (TID "Integral") tInt)
                    <:> addInst [] (IsIn (TID "Integral") tInteger)
      double_classes  = addClass (TID "Fractional") []
                    <:> addInst [] (IsIn (TID "Fractional") tDouble)
                    <:> addInst [] (IsIn (TID "Fractional") tFloat)
  describe "split" $ do
    it "works for single predicate: retains it" $ do
      let type01  = Tyvar (TID "a") Star
          Right ce = evalLogger $ (diamond_classes <:> diamond_inst) initialEnv
          fixvars = []
          toquant = [type01]
          predics = [IsIn (TID "ClassA") tInt, IsIn (TID "ClassC") tInt, IsIn (TID "ClassC") (TVar type01)]
          (Right (deferred, retained), _) = startTI $ runLoggerT (split ce fixvars toquant predics)
      retained `shouldContain` [IsIn (TID "ClassC") (TVar type01)]
      deferred `shouldBe` []
    it "works for single predicate: defers it" $ do
      let type01  = Tyvar (TID "a") Star
          Right ce = evalLogger $ (diamond_classes <:> diamond_inst) initialEnv
          fixvars = [type01]
          toquant = []
          predics = [IsIn (TID "ClassA") tInt, IsIn (TID "ClassC") tInt, IsIn (TID "ClassC") (TVar type01)]
          (Right (deferred, retained), _) = startTI $ runLoggerT (split ce fixvars toquant predics)
      deferred `shouldContain` [IsIn (TID "ClassC") (TVar type01)]
      retained `shouldBe` []
    it "works for single predicate: defaulting" $ do
      let type01  = Tyvar (TID "a") Star
          type02  = Tyvar (TID "b") Star
          Right ce = evalLogger $ (int_classes <:> double_classes) initialEnv
          fixvars = []
          toquant = []
          predics = [IsIn (TID "Integral") (TVar type01), IsIn (TID "Fractional") (TVar type02)]
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
