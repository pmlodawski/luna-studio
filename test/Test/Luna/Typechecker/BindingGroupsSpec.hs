module Test.Luna.Typechecker.BindingGroupsSpec (spec) where


import Luna.Typechecker.BindingGroups
import Luna.Typechecker.TIMonad
import Luna.Typechecker.Typeclasses

import Luna.Typechecker.AST.ClassID
import Luna.Typechecker.AST.Kind
import Luna.Typechecker.AST.Lit
import Luna.Typechecker.AST.Pat
import Luna.Typechecker.AST.Scheme
import Luna.Typechecker.AST.TID
import Luna.Typechecker.AST.Type
import Luna.Typechecker.AST.VarID

import Luna.Typechecker.Internal.Logger

import Data.Either                      (isLeft)

import Test.Hspec
import Test.Luna.Typechecker.Common


spec :: Spec
spec = do
  let lel_pat1  = [PVar (VarID "x")]
      lel_body1 = foldl1 Ap [Var (VarID "(==)"), Var (VarID "x"), Var (VarID "x")]
      Right ceEq = evalLogger $ ( addClass (ClassID "Eq") []
                 <:> addInst [] (IsIn (ClassID "Eq") tInt)
                  ) initialEnv

  describe "tiExpl" $ do
    it "fails for too weak contexts" $ do
      let inf       = tiExpl ceEq [eqBG^.asmp] (VarID "lel", lel_type, [(lel_pat1, lel_body1)])
          lel_type  = Forall [Star] ([] :=> (TGen 0 `fn` tBool))
      startTI (evalLoggerT inf) `shouldSatisfy` isLeft
    it "fails for too general signatures" $ do
      let inf       = tiExpl ceEq [eqBG^.asmp] (VarID "lel", lel_type, [(lel_pat1, lel_body1)])
          lel_type  = Forall [Star, Star] ([] :=> (TGen 0 `fn` TGen 1))
      startTI (evalLoggerT inf) `shouldSatisfy` isLeft

  describe "tiExpr" $
    it "recurses into let" $
      let Right ce = evalLogger ((  addClass (ClassID "Eq") []
                         <:> addClass (ClassID "Ord") [ClassID "Eq"]
                         <:> addClass (ClassID "Num") []
                         <:> addClass (ClassID "Real") [ClassID "Num", ClassID "Ord"]
                         <:> addClass (ClassID "Enum") []
                         <:> addClass (ClassID "Integral") [ClassID "Real", ClassID "Enum"]
                         <:> addClass (ClassID "Fractional") [ClassID "Num"]
                         <:> addClass (ClassID "Functor") []
                         <:> addInst [] (IsIn (ClassID "Eq") tInt)             <:> addInst [] (IsIn (ClassID "Eq") tInteger)       <:> addInst [] (IsIn (ClassID "Eq") tDouble)   <:> addInst [] (IsIn (ClassID "Eq") tFloat)
                         <:> addInst [] (IsIn (ClassID "Ord") tInt)            <:> addInst [] (IsIn (ClassID "Ord") tInteger)      <:> addInst [] (IsIn (ClassID "Ord") tDouble)  <:> addInst [] (IsIn (ClassID "Ord") tFloat)
                         <:> addInst [] (IsIn (ClassID "Num") tInt)            <:> addInst [] (IsIn (ClassID "Num") tInteger)      <:> addInst [] (IsIn (ClassID "Num") tDouble)  <:> addInst [] (IsIn (ClassID "Num") tFloat)
                         <:> addInst [] (IsIn (ClassID "Real") tInt)           <:> addInst [] (IsIn (ClassID "Real") tInteger)     <:> addInst [] (IsIn (ClassID "Real") tDouble) <:> addInst [] (IsIn (ClassID "Real") tFloat)
                         <:> addInst [] (IsIn (ClassID "Enum") tInt)           <:> addInst [] (IsIn (ClassID "Enum") tInteger)
                         <:> addInst [] (IsIn (ClassID "Integral") tInt)       <:> addInst [] (IsIn (ClassID "Integral") tInteger)
                         <:> addInst [] (IsIn (ClassID "Fractional") tDouble)  <:> addInst [] (IsIn (ClassID "Fractional") tFloat)
                         <:> addInst [] (IsIn (ClassID "Functor") tList)       <:> addInst [] (IsIn (ClassID "Functor") tMaybe)
                         <:> addInst [IsIn (ClassID "Functor") tv_f2, IsIn (ClassID "Num") tv_a1] (IsIn (ClassID "Num") (TAp tv_f2 tv_a1)) -- nonsense, I know
                         <:> addInst [IsIn (ClassID "Functor") tv_f2, IsIn (ClassID "Ord") tv_a1]
                                     (IsIn (ClassID "Ord") (TAp tv_f2 tv_a1))
                          ) initialEnv)

          tMaybe = TCon $ Tycon (TID "Maybe") $ Star `Kfun` Star

          tv_a1 = TVar $ Tyvar (TID "a") Star
          tv_f2 = TVar $ Tyvar (TID "f") (Star `Kfun` Star)

          as = [ integralAddBG  ^.asmp
               , consBG         ^.asmp
               , foldlBG        ^.asmp
               , sumBG          ^.asmp
               , takeBG         ^.asmp
               , zipWithBG      ^.asmp
               , fractionalDivBG^.asmp
               , iterateBG      ^.asmp
               , negateBG       ^.asmp
               , nilBG          ^.asmp
               ]
          bg = ( []
                 , [[
                      (VarID "pie", [( []
                               , ap [ Var (VarID "sum")
                                    , ap [ Var (VarID "take")
                                         , Lit (LitIntegral 2)
                                         , ap [ Var (VarID "zipWith")
                                              , Var (VarID "(/)")
                                              , ap [ Var (VarID "iterate")
                                                   , Var (VarID "negate")
                                                   , Lit (LitIntegral 4)
                                                   ]
                                              , ap [ Var (VarID "(:)")
                                                   , Lit (LitIntegral 1)
                                                   , Var (VarID "[]")
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
                    (VarID "pie2", [([], Let bg (Var (VarID "pie")))])
                  ]]
                )


          Right (ps, t@(TVar (Tyvar _ t_kind))) = startTI $ evalLoggerT $ tiExpr ce as (Let bg2 (Var (VarID "pie2")))
       in do
        t_kind `shouldBe` Star
        ps `shouldContain` [IsIn (ClassID "Integral") t]
