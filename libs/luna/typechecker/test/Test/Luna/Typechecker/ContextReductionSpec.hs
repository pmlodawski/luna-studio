module Test.Luna.Typechecker.ContextReductionSpec (spec) where

--import Luna.Typechecker.AST.Alternatives as Alt
--import Luna.Typechecker.AST.Common       as Cmm
--import Luna.Typechecker.AST.Expr         as Exp
import Luna.Typechecker.AST.Kind
--import Luna.Typechecker.AST.Lit          as Lit
--import Luna.Typechecker.AST.Module       as Mod
--import Luna.Typechecker.AST.Pat          as Pat
--import Luna.Typechecker.AST.Scheme       as Sch
--import Luna.Typechecker.AST.TID          as TID
import Luna.Typechecker.AST.Type


--import Luna.Typechecker.Ambiguity        as Amb
--import Luna.Typechecker.Assumptions      as Ass
--import Luna.Typechecker.BindingGroups    as Bnd
import Luna.Typechecker.ContextReduction
--import Luna.Typechecker.HasKind          as HKd
--import Luna.Typechecker.Substitutions    as Sub
--import Luna.Typechecker.TIMonad          as TIM
import Luna.Typechecker.Typeclasses
--import Luna.Typechecker.TypeInference    as Inf
--import Luna.Typechecker.Unification      as Uni
--import Luna.Typechecker                           as Typechecker



--import Luna.Typechecker.Assumptions
--import Luna.Typechecker.BindingGroups
--import Luna.Typechecker.Substitutions


--import Luna.Typechecker.TIMonad
--import Luna.Typechecker.AST.Lit

--import Luna.Typechecker.AST.Pat
--import Luna.Typechecker.AST.Scheme



import Test.Hspec
import Control.Exception


spec :: Spec
spec = do
  describe "inHnf" $ do
    it "verifies the result" $ do
      inHnf (IsIn "anything" (TVar $ Tyvar "a" Star)) `shouldBe` True
      inHnf (IsIn "anything" (TCon $ Tycon "Int" Star)) `shouldBe` False
      inHnf (IsIn "anything" (list (TCon $ Tycon "Int" Star))) `shouldBe` False
      inHnf (IsIn "anything" (list (TVar $ Tyvar "a" Star))) `shouldBe` False
      inHnf (IsIn "anything" (TAp (TVar $ Tyvar "m" Star) (TCon $ Tycon "Int" Star))) `shouldBe` True
      evaluate (inHnf (IsIn "anything" (TGen 0))) `shouldThrow` anyErrorCall
  describe "toHnf" $ do
    it "works for bad input" $ do
      let Just ce = (     addClass "Eq" []
                      <:> addInst [] (IsIn "Eq" tBool))
                    initialEnv
          res = toHnf initialEnv (IsIn "anything" (TCon $ Tycon "Int" Star)) :: Either String [Pred]
          res2 = toHnf ce (IsIn "Eq" tBool) :: Either String [Pred]
          res3 = toHnf ce (IsIn "Eq" tInt) :: Either String [Pred]
      evaluate res `shouldThrow` anyErrorCall
      res2 `shouldBe` Right []
      evaluate res3 `shouldThrow` anyErrorCall

    it "covers nested predicates" $ do
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
                               (IsIn "Ord" ((TAp tv_f2 tv_a1)))
                    ) initialEnv

          tMaybe = TCon $ Tycon "Maybe" $ Star `Kfun` Star

          tv_a1 = TVar $ Tyvar "a" $ Star
          tv_f2 = TVar $ Tyvar "f" $ Star `Kfun` Star

          --ap :: [Expr] -> Expr
          --ap = foldl1 Ap

              --ps = [IsIn "Functor" (TVar $ Tyvar "f" Star), IsIn "Ord" (TVar $ Tyvar "a" Star)]
              --p  = (IsIn "Ord" (TAp (TVar $ Tyvar "f" Star) (TVar $ Tyvar "a" Star)))
          --entail ce ps p

          --foldl_type  = Forall [Star, Star] ([] :=> ((TGen 1 `fn` TGen 0 `fn` TGen 1) `fn` TGen 1 `fn` list (TGen 0) `fn` TGen 1))
          --foldl_pat1  = [PWildcard, PVar "a", PCon ("[]":>:nil_type) []]
          --foldl_body1 = Var "a"
          --foldl_pat2  = [PVar "f", PVar "a", PAs "xxs" (PCon (":":>:cons_type) [PVar "x", PVar "xs"]  )]
          --foldl_body2 = ap [Var "foldl", Var "f", ap [Var "f", Var "a", Var "x"], Var "xs"]

          --integralAdd_type = Forall [Star] ([IsIn "Num" (TGen 0)] :=> (TGen 0 `fn` TGen 0 `fn` TGen 0))

          --sum_type = Forall [Star] ([IsIn "Num" (TGen 0)] :=> (list (TGen 0) `fn` TGen 0))
          --sum_pat  = []
          --sum_body = ap [Var "foldl", Var "(+)", Lit (LitInt 0)]

          --cons_type = Forall [Star] ([] :=> (TGen 0 `fn` list (TGen 0) `fn` list (TGen 0)))
          --nil_type  = Forall [Star] ([] :=> (list (TGen 0)))


          --evalTI :: Subst -> Int -> TI a -> (Subst, Int, a)
          --evalTI subst i (TI f) = f subst i


          --as = [ "(+)"    :>: integralAdd_type
          --     , "(:)"    :>: cons_type
          --     , "foldl"  :>: foldl_type
          --     , "sum"    :>: sum_type
          --     , "take"   :>: (Forall [Star          ] $ [                   ] :=> (  tInt `fn` list (TGen 0) `fn` list (TGen 0))                                                 )
          --     , "zipWith":>: (Forall [Star,Star,Star] $ [                   ] :=> (  (TGen 0 `fn` TGen 1 `fn` TGen 2) `fn` list (TGen 0) `fn` list (TGen 1) `fn` list (TGen 2))  )
          --     , "(/)"    :>: (Forall [Star          ] $ [IsIn "Num" (TGen 0)] :=> (  TGen 0 `fn` TGen 0 `fn` TGen 0)                                                             )
          --     , "iterate":>: (Forall [Star          ] $ [                   ] :=> (  (TGen 0 `fn` TGen 0) `fn` TGen 0 `fn` list (TGen 0))                                        )
          --     , "negate" :>: (Forall [Star          ] $ [IsIn "Num" (TGen 0)] :=> (  TGen 0 `fn` TGen 0)                                                                         )
          --     , "[]"     :>: nil_type
          --     ]
          --bgs = [( []
          --       , [[
          --            ("pie", [( []
          --                     , ap [ Var "sum"
          --                          , ap [ Var "take"
          --                               , Lit (LitIntegral 2)
          --                               , ap [ Var "zipWith"
          --                                    , Var "(/)"
          --                                    , ap [ Var "iterate"
          --                                         , Var "negate"
          --                                         , Lit (LitIntegral 4)
          --                                         ]
          --                                    , ap [ Var "(:)"
          --                                         , Lit (LitIntegral 1)
          --                                         , Var "[]"
          --                                         ]
          --                                    ]
          --                               ]
          --                          ]
          --                    )]
          --            )
          --         ]]
          --      )]
          --bgs = [([], [[("sum", [(sum_pat, sum_body)])]])]


          --(__s1, __i1,     (ps, as')) = evalTI nullSubst    0 $ tiSeq tiBindGroup ce as bgs
          --(__s2, __i2,     s)         = evalTI __s1      __i1 $ getSubst
          --(__s3, __i3,     rs)        = evalTI __s2      __i2 $ reduce ce (apply s ps)
          --(__s4, __i4,     s')        = evalTI __s3      __i3 $ defaultSubst ce [] rs

          --result = apply (s' @@ s) as'

          p = IsIn "Num" (TAp tMaybe tv_a1)
          --(IsIn i _) = p

          --it = insts ce i

          --[ps1 :=> h1] = take 1 $ insts ce "Num"

          Just res = toHnf ce p
       in res `shouldSatisfy` any (\(IsIn name _) -> name == "Num")


