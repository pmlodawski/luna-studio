module Test.Luna.TypecheckerSpec (spec) where

import Luna.Typechecker.AST.Kind      (Kind(..))
import Luna.Typechecker.AST.Lit       (Lit(..))
import Luna.Typechecker.AST.Pat       (Pat(..))
import Luna.Typechecker.AST.Scheme    (Scheme(..),toScheme)
import Luna.Typechecker.AST.Type      (Type(..),fn,tInteger,list,tBool,tInt,tChar)

import Luna.Typechecker.Assumptions   (Assump(..))
import Luna.Typechecker.BindingGroups (Expr(..))
import Luna.Typechecker.Typeclasses   (Qual(..), Pred(..), initialEnv, addClass, addInst, (<:>))
import Luna.Typechecker               (tiProgram)

import Test.Hspec
import Control.DeepSeq
import Control.Exception
import Test.Luna.Common

import Data.Functor.Identity

import Luna.Typechecker.Internal.Logger

spec :: Spec
spec = do
  describe "the typechecker interface for basic AST" $ do
    let 
        alternatingFBG = ( "f"
                          , toScheme (tInteger `fn` list tInteger)
                          , [ (  [PVar "x"]
                              ,  ap [EConst (consBG^.asmp), Var "x", ap [Var "g", Var "x"]]
                              )
                            ]
                          )

        alternatingGBG = ( "g"
                          , toScheme (tInteger `fn` list tInteger)
                          , [ (  [PVar "x"]
                              ,  ap [EConst (consBG^.asmp), Var "x", ap [Var "f", ap [Var "decr", Var "x"]]]
                              )
                            ]
                          )

        badtestBG      = ( "badtest"
                          , Forall [Star] ([] :=> (TGen 0 `fn` TGen 0 `fn` tBool))
                          , [ (  [PVar "x", PVar "y"]
                              ,  Let ([], [[("result", [([PWildcard], ap [Var "(==)", Var "x", Var "y"])])]]) (Var "result")
                              )
                            ]
                          )

        decrBG         = ( "decr"
                          , toScheme (tInteger `fn` tInteger)
                          , [ (  [ error "no pat for decr" ]
                              ,  error "no body for decr"
                              )
                            ]
                          )

        fultingBG      = ( "fulting"
                          , Forall [] ([] :=> (tInt `fn` tBool))
                          , [ (  [PVar "v"]
                              ,  ap [Var "(==)", Var "v", ap [Var "fromIntegral", ap [Var "(+)", Lit (LitIntegral 2), Lit (LitIntegral 3)]]]
                              )
                            ]
                          )

    it "typechecks `const`" $ do
      let def = ([ constBG ] , [])
      force (tiProgram initialEnv [] [def]) `shouldContain` [constBG^.asmp]
    

    it "infers type for `const`" $ do
      let def = ([] , [[ constBG^.impl ]] )
      force (tiProgram initialEnv [] [def]) `shouldContain` [constBG^.asmp]
    

    it "typechecks `gcd`" $ do
      let def = ( [ gcdBG ] , [] )
      tiProgram initialEnv [modBG^.asmp] [def] `shouldContain` [gcdBG^.asmp]
    

    it "infers type for `gcd`" $ do
      let def = ( [] , [[ gcdBG^.impl ]] )
      tiProgram initialEnv [modBG^.asmp] [def] `shouldContain` [gcdBG^.asmp]
    

    it "typechecks `foldr`" $ do
      let def = ([foldrBG], [])
          res = tiProgram initialEnv [] [def]
      res `shouldContain` [foldrBG^.asmp]
    

    it "infers type for `foldr`" $ do
      let def = ([], [[ foldrBG^.impl ]] )
          res = tiProgram initialEnv [] [def]
      res `shouldContain` [foldrBG^.asmp]
    

    it "typechecks `and`" $ do
      let def = ([andBG], [])
          res = tiProgram initialEnv [foldrBG^.asmp, landBG^.asmp] [def]
      res `shouldContain` ["and" :>: (andBG ^. scheme)]
    

    it "infers type for `and`" $ do
      let def = ([], [[ andBG ^. impl ]])
          res = tiProgram initialEnv [foldrBG^.asmp, landBG^.asmp] [def]
      res `shouldContain` ["and" :>: (andBG ^. scheme)]
    

    it "typechecks `foldr` & infers `and`" $ do
      let defs = [ ([], [[ foldrBG^.impl ]] )
                 , ([], [[ andBG ^. impl ]] )
                 ]
          res = tiProgram initialEnv [landBG^.asmp] defs
      res `shouldContain` [foldrBG^.asmp]
      res `shouldContain` [andBG ^. asmp]
    

    it "typechecks mutually-recursive functions" $ do
      let def = ( [ alternatingFBG, alternatingGBG ] , [])
          res = tiProgram initialEnv [decrBG^.asmp] [def]
      res `shouldContain` [alternatingFBG^.asmp]
      res `shouldContain` [alternatingGBG^.asmp]


    it "infers types for mutually-recursive functions" $ do
      let def = ( [], [[ alternatingFBG^.impl, alternatingGBG^.impl ]] )
          res = tiProgram initialEnv [decrBG^.asmp] [def]
      res `shouldContain` [alternatingFBG^.asmp]
      res `shouldContain` [alternatingGBG^.asmp]

    it "signals too general explicit typing" $ do
      let def = ( [badtestBG], [] )
          res = tiProgram initialEnv [eqBG^.asmp] [def]
      evaluate res `shouldThrow` anyErrorCall


    it "typechecks the example with mutually-recursive functions, one typed, both using typeclasses" $ do
      let def = ( [alternatingFBG], [[alternatingGBG^.impl]])
          classenvT = addClass "Eq" []
                  <:> addClass "Ord" ["Eq"]
                  <:> addInst [] (IsIn "Eq" tBool)
                  <:> addInst [] (IsIn "Ord" tBool)
          (Right classenv, _) = runIdentity $ runLoggerT $ classenvT initialEnv
          res = tiProgram classenv [decrBG^.asmp, leqBG^.asmp, eqBG^.asmp, lorBG^.asmp] [def]
      res `shouldContain` [alternatingFBG^.asmp]
      res `shouldContain` [alternatingGBG^.asmp]


    it "resolves ambiguities: `fromIntegral (2 + 3)`" $ do
      let def = ( [fultingBG] , [])
          classenvT = addClass "Eq" []
                  <:> addClass "Ord" ["Eq"]
                  <:> addClass "Num" []
                  <:> addClass "Real" ["Num", "Ord"]
                  <:> addClass "Enum" []
                  <:> addClass "Integral" ["Real", "Enum"]
                  <:> addInst [] (IsIn "Eq" tInt)       <:> addInst [] (IsIn "Eq" tInteger)
                  <:> addInst [] (IsIn "Ord" tInt)      <:> addInst [] (IsIn "Ord" tInteger)
                  <:> addInst [] (IsIn "Num" tInt)      <:> addInst [] (IsIn "Num" tInteger)
                  <:> addInst [] (IsIn "Real" tInt)     <:> addInst [] (IsIn "Real" tInteger)
                  <:> addInst [] (IsIn "Enum" tInt)     <:> addInst [] (IsIn "Enum" tInteger)
                  <:> addInst [] (IsIn "Integral" tInt) <:> addInst [] (IsIn "Integral" tInteger)
          (Right classenv, _) = runIdentity $ runLoggerT $ classenvT initialEnv
          res = tiProgram classenv [eqBG^.asmp, fromIntegralBG^.asmp, integralAddBG^.asmp] [def]
      res `shouldContain` [fultingBG^.asmp]

    it "handles monomorphism restriction" $ do
      let def pat = ([], [[("test", [(pat, test_body)])]])
          test_pat  = [PVar "x"]
          test_pat' = []
          test_body = Var "show"
          show_type = Forall [Star] ([IsIn "Show" (TGen 0)] :=> (TGen 0 `fn` list tChar))
          (Right ce, _) = runIdentity $ runLoggerT $ (  addClass "Show" []
                                                    <:> addInst [] (IsIn "Show" tInt)
                                                    <:> addInst [] (IsIn "Show" tBool)
                                                     ) initialEnv
          res pat = tiProgram ce ["show":>:show_type] [def pat]
      res test_pat `shouldContain` ["test":>:Forall [Star, Star] ([IsIn "Show" (TGen 0)] :=> (TGen 1 `fn` TGen 0 `fn` list tChar))]
      evaluate (res test_pat') `shouldThrow` anyErrorCall


  describe "(coverage booster)" $
    it "should ignore Show instances" $ do
      length (concatMap show [Star, Kfun Star Star]) `shouldSatisfy` (>0)
      length (          show [Star, Kfun (Kfun Star Star) (Kfun (Kfun Star Star) (Kfun Star Star))]) `shouldSatisfy` (>0)
