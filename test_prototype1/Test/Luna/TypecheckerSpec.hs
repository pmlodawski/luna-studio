module Test.Luna.TypecheckerSpec (spec) where


import Luna.Typechecker                 (tiProgram)

import Luna.Typechecker.Assumptions     (Assump(..))
import Luna.Typechecker.BindingGroups   (Expr(..))
import Luna.Typechecker.Typeclasses     (Qual(..),Pred(..),initialEnv,addClass,addInst,(<:>))

import Luna.Typechecker.AST.ClassID
import Luna.Typechecker.AST.Kind        (Kind(..))
import Luna.Typechecker.AST.Lit         (Lit(..))
import Luna.Typechecker.AST.Pat         (Pat(..))
import Luna.Typechecker.AST.Scheme      (Scheme(..),toScheme)
import Luna.Typechecker.AST.Type        (Type(..),fn,tInteger,list,tBool,tInt,tChar)
import Luna.Typechecker.AST.VarID

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
                          , [ (  [PVar (VarID "x")]
                              ,  ap [EConst (consBG^.asmp), Var (VarID "x"), ap [Var (VarID "g"), Var (VarID "x")]]
                              )
                            ]
                          )

        alternatingGBG = ( "g"
                          , toScheme (tInteger `fn` list tInteger)
                          , [ (  [PVar (VarID "x")]
                              ,  ap [EConst (consBG^.asmp), Var (VarID "x"), ap [Var (VarID "f"), ap [Var (VarID "decr"), Var (VarID "x")]]]
                              )
                            ]
                          )

        badtestBG      = ( "badtest"
                          , Forall [Star] ([] :=> (TGen 0 `fn` TGen 0 `fn` tBool))
                          , [ (  [PVar (VarID "x"), PVar (VarID "y")]
                              ,  Let ([], [[(VarID "result", [([PWildcard], ap [Var (VarID "(==)"), Var (VarID "x"), Var (VarID "y")])])]]) (Var (VarID "result"))
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
                          , [ (  [PVar (VarID "v")]
                              ,  ap [Var (VarID "(==)"), Var (VarID "v"), ap [Var (VarID "fromIntegral"), ap [Var (VarID "(+)"), Lit (LitIntegral 2), Lit (LitIntegral 3)]]]
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
      resultTest res (`shouldContain` [VarID "and" :>: (andBG^.scheme)])

    it "infers type for `and`" $ do
      let def = ([], [[ andBG^.impl ]])
          res = tiProgram initialEnv [foldrBG^.asmp, landBG^.asmp] [def]
      resultTest res (`shouldContain` [VarID "and" :>: (andBG^.scheme)])

    it "typechecks `foldr` & infers `and`" $ do
      let defs = [ ([], [[ foldrBG^.impl ]] )
                 , ([], [[ andBG^.impl ]] )
                 ]
          res = tiProgram initialEnv [landBG^.asmp] defs
      resultTest res (`shouldContain` [foldrBG^.asmp])
      resultTest res (`shouldContain` [andBG^.asmp])
   

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
          classenvT = addClass (ClassID "Eq") []
                  <:> addClass (ClassID "Ord") [ClassID "Eq"]
                  <:> addInst [] (IsIn (ClassID "Eq") tBool)
                  <:> addInst [] (IsIn (ClassID "Ord") tBool)
          (Right classenv, _) = runIdentity $ runLoggerT $ classenvT initialEnv
          res = tiProgram classenv [decrBG^.asmp, leqBG^.asmp, eqBG^.asmp, lorBG^.asmp] [def]
      resultTest res (`shouldContain` [alternatingFBG^.asmp])
      resultTest res (`shouldContain` [alternatingGBG^.asmp])


    it "resolves ambiguities: `fromIntegral (2 + 3)`" $ do
      let def = ( [ fultingBG^.expl ] , [])
          classenvT = addClass (ClassID "Eq") []
                  <:> addClass (ClassID "Ord") [ClassID "Eq"]
                  <:> addClass (ClassID "Num") []
                  <:> addClass (ClassID "Real") [ClassID "Num", ClassID "Ord"]
                  <:> addClass (ClassID "Enum") []
                  <:> addClass (ClassID "Integral") [ClassID "Real", ClassID "Enum"]
                  <:> addInst [] (IsIn (ClassID "Eq") tInt)       <:> addInst [] (IsIn (ClassID "Eq") tInteger)
                  <:> addInst [] (IsIn (ClassID "Ord") tInt)      <:> addInst [] (IsIn (ClassID "Ord") tInteger)
                  <:> addInst [] (IsIn (ClassID "Num") tInt)      <:> addInst [] (IsIn (ClassID "Num") tInteger)
                  <:> addInst [] (IsIn (ClassID "Real") tInt)     <:> addInst [] (IsIn (ClassID "Real") tInteger)
                  <:> addInst [] (IsIn (ClassID "Enum") tInt)     <:> addInst [] (IsIn (ClassID "Enum") tInteger)
                  <:> addInst [] (IsIn (ClassID "Integral") tInt) <:> addInst [] (IsIn (ClassID "Integral") tInteger)
          (Right classenv, _) = runIdentity $ runLoggerT $ classenvT initialEnv
          res = tiProgram classenv [eqBG^.asmp, fromIntegralBG^.asmp, integralAddBG^.asmp] [def]
      resultTest res (`shouldContain` [fultingBG^.asmp])

    it "handles monomorphism restriction" $ do
      let def pat = ([], [[(VarID "test", [(pat, test_body)])]])
          test_pat  = [PVar (VarID "x")]
          test_pat' = []
          test_body = Var (VarID "show")
          show_type = Forall [Star] ([IsIn (ClassID "Show") (TGen 0)] :=> (TGen 0 `fn` list tChar))
          (Right ce, _) = runIdentity $ runLoggerT $ (  addClass (ClassID "Show") []
                                                    <:> addInst [] (IsIn (ClassID "Show") tInt)
                                                    <:> addInst [] (IsIn (ClassID "Show") tBool)
                                                     ) initialEnv
          resF pat = tiProgram ce [VarID "show" :>: show_type] [def pat]

      resultTest (resF test_pat) (`shouldContain` [VarID "test" :>:Forall [Star, Star] ([IsIn (ClassID "Show") (TGen 0)] :=> (TGen 1 `fn` TGen 0 `fn` list tChar))])
      let (eres', _) = resF test_pat'
      eres' `shouldSatisfy` isLeft


  describe "(coverage booster)" $
    it "should ignore Show instances" $ do
      length (concatMap show [Star, Kfun Star Star]) `shouldSatisfy` (>0)
      length (          show [Star, Kfun (Kfun Star Star) (Kfun (Kfun Star Star) (Kfun Star Star))]) `shouldSatisfy` (>0)
