module Test.Luna.Typechecker.BindingGroupsSpec (spec) where

--import Luna.Typechecker.AST.Alternatives
--import Luna.Typechecker.AST.Common
--import Luna.Typechecker.AST.Expr
import Luna.Typechecker.AST.Kind
import Luna.Typechecker.AST.Lit
--import Luna.Typechecker.AST.Module
import Luna.Typechecker.AST.Pat
import Luna.Typechecker.AST.Scheme
--import Luna.Typechecker.AST.TID
import Luna.Typechecker.AST.Type


--import Luna.Typechecker.Ambiguity
import Luna.Typechecker.Assumptions
import Luna.Typechecker.BindingGroups
--import Luna.Typechecker.ContextReduction
--import Luna.Typechecker.HasKind
--import Luna.Typechecker.Substitutions
import Luna.Typechecker.TIMonad
import Luna.Typechecker.Typeclasses
--import Luna.Typechecker.TypeInference
--import Luna.Typechecker.Unification
--import Luna.Typechecker

import Test.Hspec
import Control.Exception


spec :: Spec
spec = do
  describe "tiExpl" $ do
    it "fails for too weak contexts" $ do
      let inf       = tiExpl ce ["(==)":>:eq_type] ("lel", lel_type, [(lel_pat1, lel_body1)])
          eq_type   = Forall [Star] ([IsIn "Eq"  (TGen 0)] :=> (TGen 0 `fn` TGen 0 `fn` tBool))
          lel_type  = Forall [Star] ([] :=> (TGen 0 `fn` tBool))
          lel_pat1  = [PVar "x"]
          lel_body1 = foldl1 Ap [Var "(==)", Var "x", Var "x"]
          Just ce   = (  addClass "Eq" []
                     <:> addInst [] (IsIn "Eq" tInt)
                      ) initialEnv
      evaluate (runTI inf) `shouldThrow` anyErrorCall
    it "fails for too general signatures" $ do
      let inf       = tiExpl ce ["(==)":>:eq_type] ("lel", lel_type, [(lel_pat1, lel_body1)])
          eq_type   = Forall [Star] ([IsIn "Eq"  (TGen 0)] :=> (TGen 0 `fn` TGen 0 `fn` tBool))
          lel_type  = Forall [Star, Star] ([] :=> (TGen 0 `fn` TGen 1))
          lel_pat1  = [PVar "x"]
          lel_body1 = foldl1 Ap [Var "(==)", Var "x", Var "x"]
          Just ce   = (  addClass "Eq" []
                     <:> addInst [] (IsIn "Eq" tInt)
                      ) initialEnv
      evaluate (runTI inf) `shouldThrow` anyErrorCall

  describe "tiExpr" $
    it "recurses into let" $
      let Just ce = (  addClass "Eq" []
                   <:> addClass "Ord" ["Eq"]
                   <:> addClass "Num" []
                   <:> addClass "Real" ["Num", "Ord"]
                   <:> addClass "Enum" []
                   <:> addClass "Integral" ["Real", "Enum"]
                   <:> addClass "Fractional" ["Num"]
                   <:> addClass "Functor" []
                   <:> addInst [] (IsIn "Eq" tInt)             <:> addInst [] (IsIn "Eq" tInteger)       <:> addInst [] (IsIn "Eq" tDouble)   <:> addInst [] (IsIn "Eq" tFloat)
                   <:> addInst [] (IsIn "Ord" tInt)            <:> addInst [] (IsIn "Ord" tInteger)      <:> addInst [] (IsIn "Ord" tDouble)  <:> addInst [] (IsIn "Ord" tFloat)
                   <:> addInst [] (IsIn "Num" tInt)            <:> addInst [] (IsIn "Num" tInteger)      <:> addInst [] (IsIn "Num" tDouble)  <:> addInst [] (IsIn "Num" tFloat)
                   <:> addInst [] (IsIn "Real" tInt)           <:> addInst [] (IsIn "Real" tInteger)     <:> addInst [] (IsIn "Real" tDouble) <:> addInst [] (IsIn "Real" tFloat)
                   <:> addInst [] (IsIn "Enum" tInt)           <:> addInst [] (IsIn "Enum" tInteger)
                   <:> addInst [] (IsIn "Integral" tInt)       <:> addInst [] (IsIn "Integral" tInteger)
                   <:> addInst [] (IsIn "Fractional" tDouble)  <:> addInst [] (IsIn "Fractional" tFloat)
                   <:> addInst [] (IsIn "Functor" tList)       <:> addInst [] (IsIn "Functor" tMaybe)
                   <:> addInst [IsIn "Functor" tv_f2, IsIn "Num" tv_a1] (IsIn "Num" (TAp tv_f2 tv_a1)) -- nonsense, I know
                   <:> addInst [IsIn "Functor" tv_f2, IsIn "Ord" tv_a1]
                               (IsIn "Ord" (TAp tv_f2 tv_a1))
                    ) initialEnv

          tMaybe = TCon $ Tycon "Maybe" $ Star `Kfun` Star

          tv_a1 = TVar $ Tyvar "a" Star
          tv_f2 = TVar $ Tyvar "f" (Star `Kfun` Star)

          ap :: [Expr] -> Expr
          ap = foldl1 Ap

              --ps = [IsIn "Functor" (TVar $ Tyvar "f" Star), IsIn "Ord" (TVar $ Tyvar "a" Star)]
              --p  = (IsIn "Ord" (TAp (TVar $ Tyvar "f" Star) (TVar $ Tyvar "a" Star)))
          --entail ce ps p

          foldl_type  = Forall [Star, Star] ([] :=> ((TGen 1 `fn` TGen 0 `fn` TGen 1) `fn` TGen 1 `fn` list (TGen 0) `fn` TGen 1))
          --foldl_pat1  = [PWildcard, PVar "a", PCon ("[]":>:nil_type) []]
          --foldl_body1 = Var "a"
          --foldl_pat2  = [PVar "f", PVar "a", PAs "xxs" (PCon (":":>:cons_type) [PVar "x", PVar "xs"]  )]
          --foldl_body2 = ap [Var "foldl", Var "f", ap [Var "f", Var "a", Var "x"], Var "xs"]

          integralAdd_type = Forall [Star] ([IsIn "Num" (TGen 0)] :=> (TGen 0 `fn` TGen 0 `fn` TGen 0))

          sum_type = Forall [Star] ([IsIn "Num" (TGen 0)] :=> (list (TGen 0) `fn` TGen 0))
          --sum_pat  = []
          --sum_body = ap [Var "foldl", Var "(+)", Lit (LitInt 0)]

          cons_type = Forall [Star] ([] :=> (TGen 0 `fn` list (TGen 0) `fn` list (TGen 0)))
          nil_type  = Forall [Star] ([] :=> (list (TGen 0)))


          --evalTI :: Subst -> Int -> TI a -> (Subst, Int, a)
          --evalTI subst i (TI f) = f subst i


          as = [ "(+)"     :>: integralAdd_type
               , "(:)"     :>: cons_type
               , "foldl"   :>: foldl_type
               , "sum"     :>: sum_type
               , "take"    :>: Forall [Star          ] ([                   ] :=> (  tInt `fn` list (TGen 0) `fn` list (TGen 0))                                                 )
               , "zipWith" :>: Forall [Star,Star,Star] ([                   ] :=> (  (TGen 0 `fn` TGen 1 `fn` TGen 2) `fn` list (TGen 0) `fn` list (TGen 1) `fn` list (TGen 2))  )
               , "(/)"     :>: Forall [Star          ] ([IsIn "Num" (TGen 0)] :=> (  TGen 0 `fn` TGen 0 `fn` TGen 0)                                                             )
               , "iterate" :>: Forall [Star          ] ([                   ] :=> (  (TGen 0 `fn` TGen 0) `fn` TGen 0 `fn` list (TGen 0))                                        )
               , "negate"  :>: Forall [Star          ] ([IsIn "Num" (TGen 0)] :=> (  TGen 0 `fn` TGen 0)                                                                         )
               , "[]"      :>: nil_type
               ]
          bg = ( []
                 , [[
                      ("pie", [( []
                               , ap [ Var "sum"
                                    , ap [ Var "take"
                                         , Lit (LitIntegral 2)
                                         , ap [ Var "zipWith"
                                              , Var "(/)"
                                              , ap [ Var "iterate"
                                                   , Var "negate"
                                                   , Lit (LitIntegral 4)
                                                   ]
                                              , ap [ Var "(:)"
                                                   , Lit (LitIntegral 1)
                                                   , Var "[]"
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
                    ("pie2", [([], Let bg (Var "pie"))])
                  ]]
                )
          --bgs = [bg]
          --bgs = [([], [[("sum", [(sum_pat, sum_body)])]])]


          --(__s1, __i1,     (ps, as')) = evalTI nullSubst    0 $ tiSeq tiBindGroup ce as bgs
          --(__s2, __i2,     s)         = evalTI __s1      __i1 $ getSubst
          --(__s3, __i3,     rs)        = evalTI __s2      __i2 $ reduce ce (apply s ps)
          --(__s4, __i4,     s')        = evalTI __s3      __i3 $ defaultSubst ce [] rs

          --result = apply (s' @@ s) as'

          --p = IsIn "Num" (TAp tMaybe tv_a1)
          --(IsIn i _) = p

          --it = insts ce i

          --[ps1 :=> h1] = take 1 $ insts ce "Num"


          (ps, t@(TVar (Tyvar _ t_kind))) = runTI $ tiExpr ce as (Let bg2 (Var "pie2"))
       in do
        t_kind `shouldBe` Star
        ps `shouldContain` [IsIn "Integral" t]
