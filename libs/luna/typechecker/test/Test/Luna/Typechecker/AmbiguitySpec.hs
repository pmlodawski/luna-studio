module Test.Luna.Typechecker.AmbiguitySpec (spec) where

import Luna.Typechecker.AST.Kind
import Luna.Typechecker.AST.Lit
import Luna.Typechecker.AST.Pat
import Luna.Typechecker.AST.Scheme
--import Luna.Typechecker.AST.TID
import Luna.Typechecker.AST.Type


import Luna.Typechecker.Ambiguity
import Luna.Typechecker.Assumptions
import Luna.Typechecker.BindingGroups
--import Luna.Typechecker.ContextReduction
--import Luna.Typechecker.HasKind
--import Luna.Typechecker.Substitutions
--import Luna.Typechecker.TIMonad
import Luna.Typechecker.Typeclasses
--import Luna.Typechecker.TypeInference
--import Luna.Typechecker.Unification
import Luna.Typechecker

import Test.Hspec
import Control.Exception

ap :: [Expr] -> Expr
ap = foldl1 Ap

spec :: Spec
spec = do
  let 
      --const_type = Forall [Star, Star] ([] :=> (TGen 0 `fn` TGen 1 `fn` TGen 0))  
      --const_pat  = [PVar "x", PVar "y"]
      --const_body = Var "x"

      --gcd_type  = toScheme (tInteger `fn` tInteger `fn` tInteger)
      --gcd_pat1  = [PVar "a", PLit (LitInt 0)]
      --gcd_body1 = Var "a"
      --gcd_pat2  = [PVar "a", PVar "b"]
      --gcd_body2 = ap [Var "gcd", Var "b", ap [Var "mod", Var "a", Var "b"]]
      
      --mod_type  = toScheme (tInteger `fn` tInteger `fn` tInteger)

      cons_type = Forall [Star] ([] :=> (TGen 0 `fn` list (TGen 0) `fn` list (TGen 0)))
      --nil_type  = Forall [Star] ([] :=> (list (TGen 0)))

      --foldr_type  = Forall [Star, Star] ([] :=> ((TGen 0 `fn` TGen 1 `fn` TGen 1) `fn` TGen 1 `fn` list (TGen 0) `fn` TGen 1))
      --foldr_pat1  = [PWildcard, PVar "a", PCon ("[]":>:nil_type) []]
      --foldr_body1 = Var "a"
      --foldr_pat2  = [PVar "f", PVar "a", PAs "xxs" (PCon (":":>:cons_type) [PVar "x", PVar "xs"]  )]
      --foldr_body2 = ap [Var "f", Var "x", ap [Var "foldr", Var "f", Var "a", Var "xs"]]

      --and_type = toScheme (list tBool `fn` tBool)
      --and_pat  = []
      --and_body = ap [Var "foldr", Var "(&&)", Const ("True" :>: toScheme tBool)]


      --alternating_type = toScheme (tInteger `fn` list tInteger)
      --alternating_pat  = [PVar "x"]
      --alternating1_body = ap [Const ("(:)":>:cons_type), Var "x", ap [Var "g", Var "x"]]
      --alternating2_body = ap [Const ("(:)":>:cons_type), Var "x", ap [Var "f", ap [Var "decr", Var "x"]]]

      --mutualex_type = Forall [Star] ([IsIn "Eq"  (TGen 0)] :=> (TGen 0 `fn` tBool))
      --mutualex_pat  = [PVar "x"]
      --mutualex_body = ap [Var "(||)", ap [Var "(==)", Var "x", Var "x"], ap [Var "g", Const ("True" :>: toScheme tBool)]]
      --mutualim_type = Forall [Star] ([IsIn "Ord" (TGen 0)] :=> (TGen 0 `fn` tBool))
      --mutualim_pat  = [PVar "y"]
      --mutualim_body = ap [Var "(||)", ap [Var "(<=)", Var "y", Var "y"], ap [Var "f", Const ("True" :>: toScheme tBool)]]

      --leq_type  = Forall [Star] ([IsIn "Ord" (TGen 0)] :=> (TGen 0 `fn` TGen 0 `fn` tBool))
      eq_type   = Forall [Star] ([IsIn "Eq"  (TGen 0)] :=> (TGen 0 `fn` TGen 0 `fn` tBool))
      --land_type = toScheme (tBool `fn` tBool `fn` tBool)
      --lor_type  = toScheme (tBool `fn` tBool `fn` tBool)
      fromIntegral_type = Forall [Star, Star] ([IsIn "Integral" (TGen 0), IsIn "Num" (TGen 1)] :=> (TGen 0 `fn` TGen 1))
      integralAdd_type = Forall [Star] ([IsIn "Num" (TGen 0)] :=> (TGen 0 `fn` TGen 0 `fn` TGen 0))

      fulting_type = Forall [] ([] :=> (tInt `fn` tBool))
      fulting_pat  = [PVar "v"]
      fulting_body = ap [Var "(==)", Var "v", ap [Var "fromIntegral", ap [Var "(+)", Lit (LitIntegral 2), Lit (LitIntegral 3)]]]

  describe "defaulting" $ do
    it "solves ambiguity for: `fromIntegral (2 + 3)`" $ do
      let def = ( [( "fulting_type", fulting_type, [(fulting_pat, fulting_body)])] , [])
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
          res = tiProgram classenv ["(==)":>:eq_type, "fromIntegral":>:fromIntegral_type, "(+)":>:integralAdd_type] [def]
      res `shouldContain` ["fulting_type" :>: fulting_type]

    it "can't solve ambiguity for: `fromMytype (xx my1 my2)` (I)" $ do
      let def = ( [( "fulting2_type", fulting2_type, [(fulting2_pat, fulting2_body)])] , [])
          classenvT = addClass "Eq"     []
                  <:> addClass "MyType" ["Eq"]
                  <:> addInst [] (IsIn "Eq"     tInt)
                  <:> addInst [] (IsIn "Eq"     myType1) <:> addInst [] (IsIn "Eq"     myType2)
                  <:> addInst [] (IsIn "MyType" myType1) <:> addInst [] (IsIn "MyType" myType2)

          fulting2_type = Forall [] ([] :=> (tInt `fn` tBool))
          fulting2_pat  = [PVar "v"]
          fulting2_body = ap [Var "(==)", Var "v", ap [Var "fromMytype", ap [Var "xx", Var "my1", Var "my2"]]]

          myType1 = TCon (Tycon "MyType1" Star) :: Type
          myType2 = TCon (Tycon "MyType2" Star) :: Type
          mys     = Forall [Star] ([IsIn "MyType" (TGen 0)] :=> (TGen 0))

          fromMytype_type = Forall [Star, Star] ([IsIn "MyType" (TGen 0)] :=> (TGen 0 `fn` tInt))
          xx_type         = Forall [Star] ([IsIn "MyType" (TGen 0)] :=> (TGen 0 `fn` TGen 0 `fn` TGen 0))

          Just classenv = classenvT initialEnv
          res = tiProgram classenv ["(==)":>:eq_type, "fromMytype":>:fromMytype_type, "xx":>:xx_type, "my1":>:mys, "my2":>:mys] [def]
      evaluate res `shouldThrow` anyErrorCall

    it "can't solve ambiguity for: `fromMytype (xx my1 my2)` (II)" $ do
      let def = ( [( "fulting2_type", fulting2_type, [(fulting2_pat, fulting2_body)])] , [])
          classenvT = addClass "Eq"     []
                  <:> addClass "MyType" ["Eq"]
                  <:> addInst [] (IsIn "Eq"     tInt)
                  <:> addInst [] (IsIn "Eq"     myType1) <:> addInst [] (IsIn "Eq"     myType2)
                  <:> addInst [] (IsIn "MyType" myType1) <:> addInst [] (IsIn "MyType" myType2)

          fulting2_type = Forall [] ([] :=> (tInt `fn` tBool))
          fulting2_pat  = [PVar "v"]
          fulting2_body = ap [Var "(==)", Var "v", ap [Var "fromMytype", ap [Var "xx", Var "my1", Var "my2"]]]

          myType1 = TCon (Tycon "MyType1" Star) :: Type
          myType2 = TCon (Tycon "MyType2" Star) :: Type
          mys     = Forall [Star] ([IsIn "MyType" (TGen 0)] :=> (TGen 0))

          fromMytype_type = Forall [] ([IsIn "MyType" (TVar $ Tyvar "a" Star)] :=> ((TVar $ Tyvar "a" Star) `fn` tInt))
          xx_type         = Forall [Star] ([IsIn "MyType" (TGen 0)] :=> (TGen 0 `fn` TGen 0 `fn` TGen 0))

          Just classenv = classenvT initialEnv
          res = tiProgram classenv ["(==)":>:eq_type, "fromMytype":>:fromMytype_type, "xx":>:xx_type, "my1":>:mys, "my2":>:mys] [def]
      evaluate res `shouldThrow` anyErrorCall

    it "can't solve ambiguity for: `fromMytype (xx my1 my2)` (III)" $ do
      let def = ( [( "fulting2_type", fulting2_type, [(fulting2_pat, fulting2_body)])] , [])
          classenvT = addClass "MyEq"     []
                  <:> addClass "MyType" ["MyEq"]
                  <:> addInst [] (IsIn "MyEq"     tInt)
                  <:> addInst [] (IsIn "MyEq"     myType1) <:> addInst [] (IsIn "MyEq"     myType2)
                  <:> addInst [] (IsIn "MyType" myType1) <:> addInst [] (IsIn "MyType" myType2)

          fulting2_type = Forall [] ([] :=> (tInt `fn` tBool))
          fulting2_pat  = [PVar "v"]
          fulting2_body = ap [Var "(==)", Var "v", ap [Var "fromMytype", ap [Var "xx", Var "my1", Var "my2"]]]

          myeq_type = Forall [Star] ([IsIn "MyEq" (TGen 0)] :=> (TGen 0 `fn` TGen 0 `fn` tBool))
          myType1 = TCon (Tycon "MyType1" Star) :: Type
          myType2 = TCon (Tycon "MyType2" Star) :: Type
          mys     = Forall [Star] ([IsIn "MyType" (TGen 0)] :=> (TGen 0))

          fromMytype_type = Forall [] ([IsIn "MyType" (TVar $ Tyvar "a" Star)] :=> ((TVar $ Tyvar "a" Star) `fn` tInt))
          xx_type         = Forall [Star] ([IsIn "MyType" (TGen 0)] :=> (TGen 0 `fn` TGen 0 `fn` TGen 0))

          Just classenv = classenvT initialEnv
          res = tiProgram classenv ["(==)":>:myeq_type, "fromMytype":>:fromMytype_type, "xx":>:xx_type, "my1":>:mys, "my2":>:mys] [def]
      evaluate res `shouldThrow` anyErrorCall

  describe "candidates" $ do
    it "works" $ do
      let Just ce = (  addClass "Eq"       []
                   <:> addClass "Ord"      ["Eq"]
                   <:> addClass "Num"      []
                   <:> addClass "Real"     ["Num", "Ord"]
                   <:> addClass "Enum"     []
                   <:> addClass "Integral" ["Real", "Enum"]
                   <:> addClass "Functor"     []
                   <:> addClass "Applicative" ["Functor"]
                   <:> addClass "LOLOLOL"  []

                   <:> addInst [IsIn "Functor" (TVar $ Tyvar "f" Star), IsIn "Ord" (TVar $ Tyvar "a" Star)]
                               (IsIn "Ord" ((TAp (TVar $ Tyvar "f" Star) (TVar $ Tyvar "a" Star))))
                   <:> addInst [] (IsIn "Eq"       tInt)   <:> addInst [] (IsIn "Eq"       tInteger)
                   <:> addInst [] (IsIn "Ord"      tInt)   <:> addInst [] (IsIn "Ord"      tInteger)
                   <:> addInst [] (IsIn "Num"      tInt)   <:> addInst [] (IsIn "Num"      tInteger)
                   <:> addInst [] (IsIn "Real"     tInt)   <:> addInst [] (IsIn "Real"     tInteger)
                   <:> addInst [] (IsIn "Enum"     tInt)   <:> addInst [] (IsIn "Enum"     tInteger)
                   <:> addInst [] (IsIn "Integral" tInt)   <:> addInst [] (IsIn "Integral" tInteger)
                   <:> addInst [] (IsIn "LOLOLOL"  tInt)   <:> addInst [] (IsIn "LOLOLOL"  tInteger)
                   <:> addInst [] (IsIn "Functor"     tList)
                   <:> addInst [] (IsIn "Applicative" tList)
                    ) initialEnv
          f = (TVar $ Tyvar "f" Star)
          a = (TVar $ Tyvar "a" Star)
          --v = (TVar $ Tyvar "lel" Star)
          --ps = [ IsIn "Functor" f
          --     , IsIn "Ord"     a
          --     ]
          --p  = IsIn "Ord" (TAp f a)
      candidates ce (Tyvar "a" Star, [IsIn "Ord" (TAp f a)]) `shouldBe` []
      candidates ce (Tyvar "a" Star, [IsIn "Integral" a]) `shouldBe` [tInteger]
      candidates ce (Tyvar "a" Star, [IsIn "Integral" a, IsIn "LOLOLOL" a]) `shouldBe` []


  describe "fighting monomorphism restriction" $ do
    it "works" $ do
      let as = [ "(+)":>:integralAdd_type
               , "(:)":>:cons_type
               , "foldl":>:foldl_type
               , "sum":>:sum_type
               , "take":>:(Forall [Star] $ [] :=> (tInt `fn` list (TGen 0) `fn` list (TGen 0)))
               , "zipWith":>:(Forall [Star,Star,Star] $ [] :=> ((TGen 0 `fn` TGen 1 `fn` TGen 2) `fn` list (TGen 0) `fn` list (TGen 1) `fn` list (TGen 2)))
               , "(/)":>:(Forall [Star] $ [IsIn "Num" (TGen 0)] :=> (TGen 0 `fn` TGen 0 `fn` TGen 0))
               , "iterate":>:(Forall [Star] $ [] :=> ((TGen 0 `fn` TGen 0) `fn` TGen 0 `fn` list (TGen 0)))
               , "negate":>:(Forall [Star] $ [IsIn "Num" (TGen 0)] :=> (TGen 0 `fn` TGen 0))
               , "[]":>:nil_type
               ]
          bgs = [( []
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
                )]
          foldl_type  = Forall [Star, Star] ([] :=> ((TGen 1 `fn` TGen 0 `fn` TGen 1) `fn` TGen 1 `fn` list (TGen 0) `fn` TGen 1))
          --foldl_pat1  = [PWildcard, PVar "a", PCon ("[]":>:nil_type) []]
          --foldl_body1 = Var "a"
          --foldl_pat2  = [PVar "f", PVar "a", PAs "xxs" (PCon (":":>:cons_type) [PVar "x", PVar "xs"]  )]
          --foldl_body2 = ap [Var "foldl", Var "f", ap [Var "f", Var "a", Var "x"], Var "xs"]

          --integralAdd_type = Forall [Star] ([IsIn "Num" (TGen 0)] :=> (TGen 0 `fn` TGen 0 `fn` TGen 0))

          sum_type = Forall [Star] ([IsIn "Num" (TGen 0)] :=> (list (TGen 0) `fn` TGen 0))
          --sum_pat  = []
          --sum_body = ap [Var "foldl", Var "(+)", Lit (LitInt 0)]

          --cons_type = Forall [Star] ([] :=> (TGen 0 `fn` list (TGen 0) `fn` list (TGen 0)))
          nil_type  = Forall [Star] ([] :=> (list (TGen 0)))

          Just ce = (  addClass "Eq" []
                   <:> addClass "Ord" ["Eq"]
                   <:> addClass "Num" []
                   <:> addClass "Real" ["Num", "Ord"]
                   <:> addClass "Enum" []
                   <:> addClass "Integral" ["Real", "Enum"]
                   <:> addClass "Functor" []
                   <:> addInst [] (IsIn "Eq" tInt)       <:> addInst [] (IsIn "Eq" tInteger)
                   <:> addInst [] (IsIn "Ord" tInt)      <:> addInst [] (IsIn "Ord" tInteger)
                   <:> addInst [] (IsIn "Num" tInt)      <:> addInst [] (IsIn "Num" tInteger)
                   <:> addInst [] (IsIn "Real" tInt)     <:> addInst [] (IsIn "Real" tInteger)
                   <:> addInst [] (IsIn "Enum" tInt)     <:> addInst [] (IsIn "Enum" tInteger)
                   <:> addInst [] (IsIn "Integral" tInt) <:> addInst [] (IsIn "Integral" tInteger)
                   <:> addInst [] (IsIn "Functor" tList)
                   <:> addInst [IsIn "Functor" (TVar $ Tyvar "f" Star), IsIn "Ord" (TVar $ Tyvar "a" Star)]
                               (IsIn "Ord" ((TAp (TVar $ Tyvar "f" Star) (TVar $ Tyvar "a" Star))))
                    ) initialEnv
       in tiProgram ce as bgs `shouldContain` ["pie":>:toScheme tInteger]
