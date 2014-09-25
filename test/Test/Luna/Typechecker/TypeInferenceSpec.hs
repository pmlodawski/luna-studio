module Test.Luna.Typechecker.TypeInferenceSpec (spec) where


import Luna.Typechecker.TypeInference
import Luna.Typechecker.TIMonad         (startTI)
import Luna.Typechecker.Typeclasses

import Luna.Typechecker.AST.Kind
import Luna.Typechecker.AST.Type

import Luna.Typechecker.Internal.Logger

import Test.Hspec
import Test.QuickCheck

import Test.Luna.Typechecker.AST.TypeGen


spec :: Spec
spec = do
  let diamond_classes = addClass "ClassA" []
                    <:> addClass "ClassB1" ["ClassA"]
                    <:> addClass "ClassB2" ["ClassA"]
                    <:> addClass "ClassC" ["ClassB1", "ClassB2"]
      diamond_inst    = addInst [] (IsIn "ClassB2" tInt)
                    <:> addInst [] (IsIn "ClassA" tInt)
                    <:> addInst [] (IsIn "ClassC" tInt)
                    <:> addInst [] (IsIn "ClassB1" tInt)
      int_classes     = addClass "Integral" []
                    <:> addInst [] (IsIn "Integral" tInt)
                    <:> addInst [] (IsIn "Integral" tInteger)
      double_classes  = addClass "Fractional" []
                    <:> addInst [] (IsIn "Fractional" tDouble)
                    <:> addInst [] (IsIn "Fractional" tFloat)
  describe "split" $ do
    it "works for single predicate: retains it" $ do
      let type01  = Tyvar "a" Star
          Right ce = evalLogger $ (diamond_classes <:> diamond_inst) initialEnv
          fixvars = []
          toquant = [type01]
          predics = [IsIn "ClassA" tInt, IsIn "ClassC" tInt, IsIn "ClassC" (TVar type01)]
          (Right (deferred, retained), _) = startTI $ runLoggerT (split ce fixvars toquant predics)
      retained `shouldContain` [IsIn "ClassC" (TVar type01)]
      deferred `shouldBe` []
    it "works for single predicate: defers it" $ do
      let type01  = Tyvar "a" Star
          Right ce = evalLogger $ (diamond_classes <:> diamond_inst) initialEnv
          fixvars = [type01]
          toquant = []
          predics = [IsIn "ClassA" tInt, IsIn "ClassC" tInt, IsIn "ClassC" (TVar type01)]
          (Right (deferred, retained), _) = startTI $ runLoggerT (split ce fixvars toquant predics)
      deferred `shouldContain` [IsIn "ClassC" (TVar type01)]
      retained `shouldBe` []
    it "works for single predicate: defaulting" $ do
      let type01  = Tyvar "a" Star
          type02  = Tyvar "b" Star
          Right ce = evalLogger $ (int_classes <:> double_classes) initialEnv
          fixvars = []
          toquant = []
          predics = [IsIn "Integral" (TVar type01), IsIn "Fractional" (TVar type02)]
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
