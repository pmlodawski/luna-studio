module Test.Luna.Typechecker.TypeInferenceSpec (spec) where

--import Luna.Typechecker.AST.Alternatives ()
--import Luna.Typechecker.AST.Common       ()
--import Luna.Typechecker.AST.Expr         ()
--import Luna.Typechecker.AST.Kind         ()
--import Luna.Typechecker.AST.Lit          ()
--import Luna.Typechecker.AST.Module       ()
--import Luna.Typechecker.AST.Pat          ()
--import Luna.Typechecker.AST.Scheme       ()
--import Luna.Typechecker.AST.TID          ()
--import Luna.Typechecker.AST.Type         ()


--import Luna.Typechecker.Ambiguity        ()
--import Luna.Typechecker.Assumptions      ()
--import Luna.Typechecker.BindingGroups    ()
--import Luna.Typechecker.ContextReduction ()
--import Luna.Typechecker.HasKind          ()
--import Luna.Typechecker.Substitutions    ()
--import Luna.Typechecker.TIMonad          ()
--import Luna.Typechecker.Typeclasses      ()
--import Luna.Typechecker.TypeInference    ()
--import Luna.Typechecker.Unification      ()
--import Luna.Typechecker                           ()

import Luna.Typechecker.TypeInference
import Luna.Typechecker.TIMonad (runTI)
import Luna.Typechecker.Typeclasses
import Test.Luna.Typechecker.AST.TypeGen
import Luna.Typechecker.AST.Kind
import Luna.Typechecker.AST.Type

import Test.Hspec
import Test.QuickCheck

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
    --it "is" pending
    it "works for single predicate: retains it" $ do
      let type01  = Tyvar "a" Star
          Just ce = (diamond_classes <:> diamond_inst) initialEnv
          fixvars = []
          toquant = [type01]
          predics = [IsIn "ClassA" tInt, IsIn "ClassC" tInt, IsIn "ClassC" (TVar type01)]
          (deferred, retained) = runTI (split ce fixvars toquant predics)
      retained `shouldContain` [IsIn "ClassC" (TVar type01)]
      deferred `shouldBe` []
    it "works for single predicate: defers it" $ do
      let type01  = Tyvar "a" Star
          Just ce = (diamond_classes <:> diamond_inst) initialEnv
          fixvars = [type01]
          toquant = []
          predics = [IsIn "ClassA" tInt, IsIn "ClassC" tInt, IsIn "ClassC" (TVar type01)]
          (deferred, retained) = runTI (split ce fixvars toquant predics)
      deferred `shouldContain` [IsIn "ClassC" (TVar type01)]
      retained `shouldBe` []
    it "works for single predicate: defaulting" $ do
      let type01  = Tyvar "a" Star
          type02  = Tyvar "b" Star
          Just ce = (int_classes <:> double_classes) initialEnv
          fixvars = []
          toquant = []
          predics = [IsIn "Integral" (TVar type01), IsIn "Fractional" (TVar type02)]
          (deferred, retained) = runTI (split ce fixvars toquant predics)
      deferred `shouldBe` []
      retained `shouldBe` []
    it "prop: returns [] for null predicate list" $
      property $
      forAll (listOf $ genTyvar Star) $ \fs ->
      forAll (listOf $ genTyvar Star) $ \gs ->
        let (ds, rs) = runTI (split initialEnv fs gs [])
            in do ds `shouldBe` []
                  rs `shouldBe` []
