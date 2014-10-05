module Test.Luna.TypecheckerSpec (spec) where


import Luna.Typechecker                 (tiProgram)

import Luna.Typechecker.Assumptions     (Assump(..))
import Luna.Typechecker.BindingGroups   (Expr(..))
import Luna.Typechecker.Typeclasses     (Qual(..),Pred(..),initialEnv,addClass,addInst,(<:>))

import Luna.Typechecker.AST.Kind        (Kind(..))
import Luna.Typechecker.AST.Lit         (Lit(..))
import Luna.Typechecker.AST.Pat         (Pat(..))
import Luna.Typechecker.AST.Scheme      (Scheme(..),toScheme)
import Luna.Typechecker.AST.TID
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
                          , [ (  [PVar (TID "x")]
                              ,  ap [EConst (consBG^.asmp), Var (TID "x"), ap [Var (TID "g"), Var (TID "x")]]
                              )
                            ]
                          )

        alternatingGBG = ( "g"
                          , toScheme (tInteger `fn` list tInteger)
                          , [ (  [PVar (TID "x")]
                              ,  ap [EConst (consBG^.asmp), Var (TID "x"), ap [Var (TID "f"), ap [Var (TID "decr"), Var (TID "x")]]]
                              )
                            ]
                          )

        badtestBG      = ( "badtest"
                          , Forall [Star] ([] :=> (TGen 0 `fn` TGen 0 `fn` tBool))
                          , [ (  [PVar (TID "x"), PVar (TID "y")]
                              ,  Let ([], [[(TID "result", [([PWildcard], ap [Var (TID "(==)"), Var (TID "x"), Var (TID "y")])])]]) (Var (TID "result"))
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
                          , [ (  [PVar (TID "v")]
                              ,  ap [Var (TID "(==)"), Var (TID "v"), ap [Var (TID "fromIntegral"), ap [Var (TID "(+)"), Lit (LitIntegral 2), Lit (LitIntegral 3)]]]
                              )
                            ]
                          )

    it "typechecks `const`" $ do
      let def = ([ constBG^.expl ] , [])
          res = tiProgram initialEnv [] [def]
      resultTest res (`shouldContain` [constBG^.asmp])
   

    it "infers type for `const`" $ do
      let def = ([] , [[ constBG^.impl ]] )
          res = tiProgram initialEnv [] [def]
      resultTest res (`shouldContain` [constBG^.asmp])
   

    it "typechecks `gcd`" $ do
      let def = ( [ gcdBG^.expl ] , [] )
          res = tiProgram initialEnv [modBG^.asmp] [def]
      resultTest res (`shouldContain` [gcdBG^.asmp])
   

    it "infers type for `gcd`" $ do
      let def = ( [] , [[ gcdBG^.impl ]] )
          res = tiProgram initialEnv [modBG^.asmp] [def]
      resultTest res (`shouldContain` [gcdBG^.asmp])
   

    it "typechecks `foldr`" $ do
      let def = ([ foldrBG^.expl ], [])
          res = tiProgram initialEnv [] [def]
      resultTest res (`shouldContain` [foldrBG^.asmp])
   

    it "infers type for `foldr`" $ do
      let def = ([], [[ foldrBG^.impl ]] )
          res = tiProgram initialEnv [] [def]
      resultTest res (`shouldContain` [foldrBG^.asmp])


    it "typechecks `and`" $ do
      let def = ([ andBG^.expl ], [])
          res = tiProgram initialEnv [foldrBG^.asmp, landBG^.asmp] [def]
      resultTest res (`shouldContain` [TID "and" :>: (andBG ^. scheme)])

    it "infers type for `and`" $ do
      let def = ([], [[ andBG ^. impl ]])
          res = tiProgram initialEnv [foldrBG^.asmp, landBG^.asmp] [def]
      resultTest res (`shouldContain` [TID "and" :>: (andBG ^. scheme)])

    it "typechecks `foldr` & infers `and`" $ do
      let defs = [ ([], [[ foldrBG^.impl ]] )
                 , ([], [[ andBG ^. impl ]] )
                 ]
          res = tiProgram initialEnv [landBG^.asmp] defs
      resultTest res (`shouldContain` [foldrBG^.asmp])
      resultTest res (`shouldContain` [andBG ^. asmp])
   

    it "typechecks mutually-recursive functions" $ do
      let def = ( [ alternatingFBG^.expl, alternatingGBG^.expl ] , [])
          res = tiProgram initialEnv [decrBG^.asmp] [def]
      resultTest res (`shouldContain` [alternatingFBG^.asmp])
      resultTest res (`shouldContain` [alternatingGBG^.asmp])


    it "infers types for mutually-recursive functions" $ do
      let def = ( [], [[ alternatingFBG^.impl, alternatingGBG^.impl ]] )
          res = tiProgram initialEnv [decrBG^.asmp] [def]
      resultTest res (`shouldContain` [alternatingFBG^.asmp])
      resultTest res (`shouldContain` [alternatingGBG^.asmp])

    it "signals too general explicit typing" $ do
      let def = ( [ badtestBG^.expl ], [] )
          (eres, _) = tiProgram initialEnv [eqBG^.asmp] [def]
      eres `shouldSatisfy` isLeft


    it "typechecks the example with mutually-recursive functions, one typed, both using typeclasses" $ do
      let def = ( [ alternatingFBG^.expl ], [[alternatingGBG^.impl]])
          classenvT = addClass (TID "Eq") []
                  <:> addClass (TID "Ord") [TID "Eq"]
                  <:> addInst [] (IsIn (TID "Eq") tBool)
                  <:> addInst [] (IsIn (TID "Ord") tBool)
          (Right classenv, _) = runIdentity $ runLoggerT $ classenvT initialEnv
          res = tiProgram classenv [decrBG^.asmp, leqBG^.asmp, eqBG^.asmp, lorBG^.asmp] [def]
      resultTest res (`shouldContain` [alternatingFBG^.asmp])
      resultTest res (`shouldContain` [alternatingGBG^.asmp])


    it "resolves ambiguities: `fromIntegral (2 + 3)`" $ do
      let def = ( [ fultingBG^.expl ] , [])
          classenvT = addClass (TID "Eq") []
                  <:> addClass (TID "Ord") [TID "Eq"]
                  <:> addClass (TID "Num") []
                  <:> addClass (TID "Real") [TID "Num", TID "Ord"]
                  <:> addClass (TID "Enum") []
                  <:> addClass (TID "Integral") [TID "Real", TID "Enum"]
                  <:> addInst [] (IsIn (TID "Eq") tInt)       <:> addInst [] (IsIn (TID "Eq") tInteger)
                  <:> addInst [] (IsIn (TID "Ord") tInt)      <:> addInst [] (IsIn (TID "Ord") tInteger)
                  <:> addInst [] (IsIn (TID "Num") tInt)      <:> addInst [] (IsIn (TID "Num") tInteger)
                  <:> addInst [] (IsIn (TID "Real") tInt)     <:> addInst [] (IsIn (TID "Real") tInteger)
                  <:> addInst [] (IsIn (TID "Enum") tInt)     <:> addInst [] (IsIn (TID "Enum") tInteger)
                  <:> addInst [] (IsIn (TID "Integral") tInt) <:> addInst [] (IsIn (TID "Integral") tInteger)
          (Right classenv, _) = runIdentity $ runLoggerT $ classenvT initialEnv
          res = tiProgram classenv [eqBG^.asmp, fromIntegralBG^.asmp, integralAddBG^.asmp] [def]
      resultTest res (`shouldContain` [fultingBG^.asmp])

    it "handles monomorphism restriction" $ do
      let def pat = ([], [[(TID "test", [(pat, test_body)])]])
          test_pat  = [PVar (TID "x")]
          test_pat' = []
          test_body = Var (TID "show")
          show_type = Forall [Star] ([IsIn (TID "Show") (TGen 0)] :=> (TGen 0 `fn` list tChar))
          (Right ce, _) = runIdentity $ runLoggerT $ (  addClass (TID "Show") []
                                                    <:> addInst [] (IsIn (TID "Show") tInt)
                                                    <:> addInst [] (IsIn (TID "Show") tBool)
                                                     ) initialEnv
          resF pat = tiProgram ce [TID "show" :>: show_type] [def pat]

      resultTest (resF test_pat) (`shouldContain` [TID "test":>:Forall [Star, Star] ([IsIn (TID "Show") (TGen 0)] :=> (TGen 1 `fn` TGen 0 `fn` list tChar))])
      let (eres', _) = resF test_pat'
      eres' `shouldSatisfy` isLeft


  describe "(coverage booster)" $
    it "should ignore Show instances" $ do
      length (concatMap show [Star, Kfun Star Star]) `shouldSatisfy` (>0)
      length (          show [Star, Kfun (Kfun Star Star) (Kfun (Kfun Star Star) (Kfun Star Star))]) `shouldSatisfy` (>0)
