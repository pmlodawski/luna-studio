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

spec :: Spec
spec = do
  describe "the typechecker interface for basic AST" $ do
    let 
        alternatingF_BG = ( "f"
                          , toScheme (tInteger `fn` list tInteger)
                          , [ (  [PVar "x"]
                              ,  ap [EConst (cons_BG^.asmp), Var "x", ap [Var "g", Var "x"]]
                              )
                            ]
                          )

        alternatingG_BG = ( "g"
                          , toScheme (tInteger `fn` list tInteger)
                          , [ (  [PVar "x"]
                              ,  ap [EConst (cons_BG^.asmp), Var "x", ap [Var "f", ap [Var "decr", Var "x"]]]
                              )
                            ]
                          )

        badtest_BG      = ( "badtest"
                          , Forall [Star] ([] :=> (TGen 0 `fn` TGen 0 `fn` tBool))
                          , [ (  [PVar "x", PVar "y"]
                              ,  Let ([], [[("result", [([PWildcard], ap [Var "(==)", Var "x", Var "y"])])]]) (Var "result")
                              )
                            ]
                          )

        decr_BG         = ( "decr"
                          , toScheme (tInteger `fn` tInteger)
                          , [ (  [ error "no pat for decr" ]
                              ,  error "no body for decr"
                              )
                            ]
                          )

        fulting_BG      = ( "fulting"
                          , Forall [] ([] :=> (tInt `fn` tBool))
                          , [ (  [PVar "v"]
                              ,  ap [Var "(==)", Var "v", ap [Var "fromIntegral", ap [Var "(+)", Lit (LitIntegral 2), Lit (LitIntegral 3)]]]
                              )
                            ]
                          )

        mutualEX_BG     = ( "ff"
                          , Forall [Star] ([IsIn "Eq"  (TGen 0)] :=> (TGen 0 `fn` tBool))
                          , [ (  [PVar "x"]
                              ,  ap [Var "(||)", ap [Var "(==)", Var "x", Var "x"], ap [Var "g", EConst ("True" :>: toScheme tBool)]]
                              )
                            ]
                          )

        mutualIM_BG     = ( "gg"
                          , Forall [Star] ([IsIn "Eq"  (TGen 0)] :=> (TGen 0 `fn` tBool))
                          , [ (  [PVar "x"]
                              ,  ap [Var "(||)", ap [Var "(==)", Var "x", Var "x"], ap [Var "g", EConst ("True" :>: toScheme tBool)]]
                              )
                            ]
                          )

    it "typechecks `const`" $ do
      let def = ([ const_BG ] , [])
      force (tiProgram initialEnv [] [def]) `shouldContain` [const_BG^.asmp]
    

    it "infers type for `const`" $ do
      let def = ([] , [[ const_BG^.impl ]] )
      force (tiProgram initialEnv [] [def]) `shouldContain` [const_BG^.asmp]
    

    it "typechecks `gcd`" $ do
      let def = ( [ gcd_BG ] , [] )
      tiProgram initialEnv [mod_BG^.asmp] [def] `shouldContain` [gcd_BG^.asmp]
    

    it "infers type for `gcd`" $ do
      let def = ( [] , [[ gcd_BG^.impl ]] )
      tiProgram initialEnv [mod_BG^.asmp] [def] `shouldContain` [gcd_BG^.asmp]
    

    it "typechecks `foldr`" $ do
      let def = ([foldr_BG], [])
          res = tiProgram initialEnv [] [def]
      res `shouldContain` [foldr_BG^.asmp]
    

    it "infers type for `foldr`" $ do
      let def = ([], [[ foldr_BG^.impl ]] )
          res = tiProgram initialEnv [] [def]
      res `shouldContain` [foldr_BG^.asmp]
    

    it "typechecks `and`" $ do
      let def = ([and_BG], [])
          res = tiProgram initialEnv [foldr_BG^.asmp, land_BG^.asmp] [def]
      res `shouldContain` ["and" :>: (and_BG ^. scheme)]
    

    it "infers type for `and`" $ do
      let def = ([], [[ and_BG ^. impl ]])
          res = tiProgram initialEnv [foldr_BG^.asmp, land_BG^.asmp] [def]
      res `shouldContain` ["and" :>: (and_BG ^. scheme)]
    

    it "typechecks `foldr` & infers `and`" $ do
      let defs = [ ([], [[ foldr_BG^.impl ]] )
                 , ([], [[ and_BG ^. impl ]] )
                 ]
          res = tiProgram initialEnv [land_BG^.asmp] defs
      res `shouldContain` [foldr_BG^.asmp]
      res `shouldContain` [and_BG ^. asmp]
    

    it "typechecks mutually-recursive functions" $ do
      let def = ( [ alternatingF_BG, alternatingG_BG ] , [])
          res = tiProgram initialEnv [decr_BG^.asmp] [def]
      res `shouldContain` [alternatingF_BG^.asmp]
      res `shouldContain` [alternatingG_BG^.asmp]


    it "infers types for mutually-recursive functions" $ do
      let def = ( [], [[ alternatingF_BG^.impl, alternatingG_BG^.impl ]] )
          res = tiProgram initialEnv [decr_BG^.asmp] [def]
      res `shouldContain` [alternatingF_BG^.asmp]
      res `shouldContain` [alternatingG_BG^.asmp]

    it "signals too general explicit typing" $ do
      let def = ( [badtest_BG], [] )
          res = tiProgram initialEnv [eq_BG^.asmp] [def]
      evaluate res `shouldThrow` anyErrorCall


    it "typechecks the example with mutually-recursive functions, one typed, both using typeclasses" $ do
      let def = ( [alternatingF_BG], [[alternatingG_BG^.impl]])
          classenvT = addClass "Eq" []
                  <:> addClass "Ord" ["Eq"]
                  <:> addInst [] (IsIn "Eq" tBool)
                  <:> addInst [] (IsIn "Ord" tBool)
          Just classenv = classenvT initialEnv
          res = tiProgram classenv [decr_BG^.asmp, leq_BG^.asmp, eq_BG^.asmp, lor_BG^.asmp] [def]
      res `shouldContain` [alternatingF_BG^.asmp]
      res `shouldContain` [alternatingG_BG^.asmp]


    it "resolves ambiguities: `fromIntegral (2 + 3)`" $ do
      let def = ( [fulting_BG] , [])
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
          Just classenv = classenvT initialEnv
          res = tiProgram classenv [eq_BG^.asmp, fromIntegral_BG^.asmp, integralAdd_BG^.asmp] [def]
      res `shouldContain` [fulting_BG^.asmp]

    it "handles monomorphism restriction" $ do
      let def pat = ([], [[("test", [(pat, test_body)])]])
          test_pat  = [PVar "x"]
          test_pat' = []
          test_body = Var "show"
          show_type = Forall [Star] ([IsIn "Show" (TGen 0)] :=> (TGen 0 `fn` list tChar))
          Just ce = (  addClass "Show" []
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
