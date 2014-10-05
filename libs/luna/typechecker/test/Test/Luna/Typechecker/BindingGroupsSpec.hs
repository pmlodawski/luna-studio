module Test.Luna.Typechecker.BindingGroupsSpec (spec) where


import Luna.Typechecker.BindingGroups
import Luna.Typechecker.TIMonad
import Luna.Typechecker.Typeclasses

import Luna.Typechecker.AST.Kind
import Luna.Typechecker.AST.Lit
import Luna.Typechecker.AST.Pat
import Luna.Typechecker.AST.Scheme
import Luna.Typechecker.AST.TID
import Luna.Typechecker.AST.Type

import Luna.Typechecker.Internal.Logger

import Data.Either                      (isLeft)

import Test.Hspec
import Test.Luna.Typechecker.Common


spec :: Spec
spec = do
  let lel_pat1  = [PVar (TID "x")]
      lel_body1 = foldl1 Ap [Var (TID "(==)"), Var (TID "x"), Var (TID "x")]
      Right ceEq = evalLogger $ ( addClass (TID "Eq") []
                 <:> addInst [] (IsIn (TID "Eq") tInt)
                  ) initialEnv

  describe "tiExpl" $ do
    it "fails for too weak contexts" $ do
      let inf       = tiExpl ceEq [eqBG^.asmp] (TID "lel", lel_type, [(lel_pat1, lel_body1)])
          lel_type  = Forall [Star] ([] :=> (TGen 0 `fn` tBool))
      startTI (evalLoggerT inf) `shouldSatisfy` isLeft
    it "fails for too general signatures" $ do
      let inf       = tiExpl ceEq [eqBG^.asmp] (TID "lel", lel_type, [(lel_pat1, lel_body1)])
          lel_type  = Forall [Star, Star] ([] :=> (TGen 0 `fn` TGen 1))
      startTI (evalLoggerT inf) `shouldSatisfy` isLeft

  describe "tiExpr" $
    it "recurses into let" $
      let Right ce = evalLogger ((  addClass (TID "Eq") []
                         <:> addClass (TID "Ord") [TID "Eq"]
                         <:> addClass (TID "Num") []
                         <:> addClass (TID "Real") [TID "Num", TID "Ord"]
                         <:> addClass (TID "Enum") []
                         <:> addClass (TID "Integral") [TID "Real", TID "Enum"]
                         <:> addClass (TID "Fractional") [TID "Num"]
                         <:> addClass (TID "Functor") []
                         <:> addInst [] (IsIn (TID "Eq") tInt)             <:> addInst [] (IsIn (TID "Eq") tInteger)       <:> addInst [] (IsIn (TID "Eq") tDouble)   <:> addInst [] (IsIn (TID "Eq") tFloat)
                         <:> addInst [] (IsIn (TID "Ord") tInt)            <:> addInst [] (IsIn (TID "Ord") tInteger)      <:> addInst [] (IsIn (TID "Ord") tDouble)  <:> addInst [] (IsIn (TID "Ord") tFloat)
                         <:> addInst [] (IsIn (TID "Num") tInt)            <:> addInst [] (IsIn (TID "Num") tInteger)      <:> addInst [] (IsIn (TID "Num") tDouble)  <:> addInst [] (IsIn (TID "Num") tFloat)
                         <:> addInst [] (IsIn (TID "Real") tInt)           <:> addInst [] (IsIn (TID "Real") tInteger)     <:> addInst [] (IsIn (TID "Real") tDouble) <:> addInst [] (IsIn (TID "Real") tFloat)
                         <:> addInst [] (IsIn (TID "Enum") tInt)           <:> addInst [] (IsIn (TID "Enum") tInteger)
                         <:> addInst [] (IsIn (TID "Integral") tInt)       <:> addInst [] (IsIn (TID "Integral") tInteger)
                         <:> addInst [] (IsIn (TID "Fractional") tDouble)  <:> addInst [] (IsIn (TID "Fractional") tFloat)
                         <:> addInst [] (IsIn (TID "Functor") tList)       <:> addInst [] (IsIn (TID "Functor") tMaybe)
                         <:> addInst [IsIn (TID "Functor") tv_f2, IsIn (TID "Num") tv_a1] (IsIn (TID "Num") (TAp tv_f2 tv_a1)) -- nonsense, I know
                         <:> addInst [IsIn (TID "Functor") tv_f2, IsIn (TID "Ord") tv_a1]
                                     (IsIn (TID "Ord") (TAp tv_f2 tv_a1))
                          ) initialEnv)

          tMaybe = TCon $ Tycon (TID "Maybe") $ Star `Kfun` Star

          tv_a1 = TVar $ Tyvar (TID "a") Star
          tv_f2 = TVar $ Tyvar (TID "f") (Star `Kfun` Star)

          as = [ integralAddBG   ^. asmp
               , consBG          ^. asmp
               , foldlBG         ^. asmp
               , sumBG           ^. asmp
               , takeBG          ^. asmp
               , zipWithBG       ^. asmp
               , fractionalDivBG ^. asmp
               , iterateBG       ^. asmp
               , negateBG        ^. asmp
               , nilBG           ^. asmp
               ]
          bg = ( []
                 , [[
                      (TID "pie", [( []
                               , ap [ Var (TID "sum")
                                    , ap [ Var (TID "take")
                                         , Lit (LitIntegral 2)
                                         , ap [ Var (TID "zipWith")
                                              , Var (TID "(/)")
                                              , ap [ Var (TID "iterate")
                                                   , Var (TID "negate")
                                                   , Lit (LitIntegral 4)
                                                   ]
                                              , ap [ Var (TID "(:)")
                                                   , Lit (LitIntegral 1)
                                                   , Var (TID "[]")
                                                   ]
                                              ]
                                         ]
                                    ]
                              )]
                      )
                   ]]
                )
          bg2 = ( []
                , [[
                    (TID "pie2", [([], Let bg (Var (TID "pie")))])
                  ]]
                )


          Right (ps, t@(TVar (Tyvar _ t_kind))) = startTI $ evalLoggerT $ tiExpr ce as (Let bg2 (Var (TID "pie2")))
       in do
        t_kind `shouldBe` Star
        ps `shouldContain` [IsIn (TID "Integral") t]
