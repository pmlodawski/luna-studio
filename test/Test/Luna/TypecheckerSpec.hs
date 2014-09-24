module Test.Luna.TypecheckerSpec (spec) where


import Luna.Typechecker                 (tiProgram)

import Luna.Typechecker.Assumptions     (Assump(..))
import Luna.Typechecker.BindingGroups   (Expr(..))
import Luna.Typechecker.Typeclasses     (Qual(..),Pred(..),initialEnv,addClass,addInst,(<:>))

import Luna.Typechecker.AST.Kind        (Kind(..))
import Luna.Typechecker.AST.Lit         (Lit(..))
import Luna.Typechecker.AST.Pat         (Pat(..))
import Luna.Typechecker.AST.Scheme      (Scheme(..),toScheme)
import Luna.Typechecker.AST.Type        (Type(..),fn,tInteger,list,tBool,tInt,tChar)

import Luna.Typechecker.Internal.Logger

import Data.Either                      (isLeft,isRight)

import Test.Hspec

import Test.Luna.Typechecker.Common

import Data.Functor.Identity


resultTest :: (Show a, Show b) => (Either a b, c) -> (b -> Expectation) -> Expectation
resultTest (eres, _) test = case eres of
                              Left _ -> eres `shouldSatisfy` isRight
                              Right res -> test res


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
          res = tiProgram initialEnv [] [def]
      resultTest res (`shouldContain` [constBG^.asmp])
    

    it "infers type for `const`" $ do
      let def = ([] , [[ constBG^.impl ]] )
          res = tiProgram initialEnv [] [def]
      resultTest res (`shouldContain` [constBG^.asmp])
    

    it "typechecks `gcd`" $ do
      let def = ( [ gcdBG ] , [] )
          res = tiProgram initialEnv [modBG^.asmp] [def]
      resultTest res (`shouldContain` [gcdBG^.asmp])
    

    it "infers type for `gcd`" $ do
      let def = ( [] , [[ gcdBG^.impl ]] )
          res = tiProgram initialEnv [modBG^.asmp] [def]
      resultTest res (`shouldContain` [gcdBG^.asmp])
    

    it "typechecks `foldr`" $ do
      let def = ([foldrBG], [])
          res = tiProgram initialEnv [] [def]
      resultTest res (`shouldContain` [foldrBG^.asmp])
    

    it "infers type for `foldr`" $ do
      let def = ([], [[ foldrBG^.impl ]] )
          res = tiProgram initialEnv [] [def]
      resultTest res (`shouldContain` [foldrBG^.asmp])


    it "typechecks `and`" $ do
      let def = ([andBG], [])
          res = tiProgram initialEnv [foldrBG^.asmp, landBG^.asmp] [def]
      resultTest res (`shouldContain` ["and" :>: (andBG ^. scheme)])

    it "infers type for `and`" $ do
      let def = ([], [[ andBG ^. impl ]])
          res = tiProgram initialEnv [foldrBG^.asmp, landBG^.asmp] [def]
      resultTest res (`shouldContain` ["and" :>: (andBG ^. scheme)])

    it "typechecks `foldr` & infers `and`" $ do
      let defs = [ ([], [[ foldrBG^.impl ]] )
                 , ([], [[ andBG ^. impl ]] )
                 ]
          res = tiProgram initialEnv [landBG^.asmp] defs
      resultTest res (`shouldContain` [foldrBG^.asmp])
      resultTest res (`shouldContain` [andBG ^. asmp])
    

    it "typechecks mutually-recursive functions" $ do
      let def = ( [ alternatingFBG, alternatingGBG ] , [])
          res = tiProgram initialEnv [decrBG^.asmp] [def]
      resultTest res (`shouldContain` [alternatingFBG^.asmp])
      resultTest res (`shouldContain` [alternatingGBG^.asmp])


    it "infers types for mutually-recursive functions" $ do
      let def = ( [], [[ alternatingFBG^.impl, alternatingGBG^.impl ]] )
          res = tiProgram initialEnv [decrBG^.asmp] [def]
      resultTest res (`shouldContain` [alternatingFBG^.asmp])
      resultTest res (`shouldContain` [alternatingGBG^.asmp])

    it "signals too general explicit typing" $ do
      let def = ( [badtestBG], [] )
          (eres, _) = tiProgram initialEnv [eqBG^.asmp] [def]
      eres `shouldSatisfy` isLeft


    it "typechecks the example with mutually-recursive functions, one typed, both using typeclasses" $ do
      let def = ( [alternatingFBG], [[alternatingGBG^.impl]])
          classenvT = addClass "Eq" []
                  <:> addClass "Ord" ["Eq"]
                  <:> addInst [] (IsIn "Eq" tBool)
                  <:> addInst [] (IsIn "Ord" tBool)
          (Right classenv, _) = runIdentity $ runLoggerT $ classenvT initialEnv
          res = tiProgram classenv [decrBG^.asmp, leqBG^.asmp, eqBG^.asmp, lorBG^.asmp] [def]
      resultTest res (`shouldContain` [alternatingFBG^.asmp])
      resultTest res (`shouldContain` [alternatingGBG^.asmp])


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
      resultTest res (`shouldContain` [fultingBG^.asmp])

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
          resF pat = tiProgram ce ["show":>:show_type] [def pat]

      resultTest (resF test_pat) (`shouldContain` ["test":>:Forall [Star, Star] ([IsIn "Show" (TGen 0)] :=> (TGen 1 `fn` TGen 0 `fn` list tChar))])
      let (eres', _) = resF test_pat'
      eres' `shouldSatisfy` isLeft


  describe "(coverage booster)" $
    it "should ignore Show instances" $ do
      length (concatMap show [Star, Kfun Star Star]) `shouldSatisfy` (>0)
      length (          show [Star, Kfun (Kfun Star Star) (Kfun (Kfun Star Star) (Kfun Star Star))]) `shouldSatisfy` (>0)
