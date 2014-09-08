module Test.Luna.TypecheckerSpec (spec) where

import Luna.Typechecker.Internal.AST.Kind      (Kind(..))
import Luna.Typechecker.Internal.AST.Lit       (Lit(..))
import Luna.Typechecker.Internal.AST.Pat       (Pat(..))
import Luna.Typechecker.Internal.AST.Scheme    (Scheme(..),toScheme)
import Luna.Typechecker.Internal.AST.Type      (Type(..),fn,tInteger,list,tBool,tInt)

import Luna.Typechecker.Internal.Assumptions   (Assump(..))
import Luna.Typechecker.Internal.BindingGroups (Expr(..))
import Luna.Typechecker.Internal.Typeclasses   (Qual(..), Pred(..), initialEnv, addClass, addInst, (<:>))
import Luna.Typechecker                        (tiProgram)

import Test.Hspec

ap :: [Expr] -> Expr
ap = foldl1 Ap

spec :: Spec
spec = do
  describe "the typechecker interface for basic AST" $ do
    let const_type = Forall [Star, Star] ([] :=> (TGen 0 `fn` TGen 1 `fn` TGen 0))  
        const_pat  = [PVar "x", PVar "y"]
        const_body = Var "x"

        gcd_type  = toScheme (tInteger `fn` tInteger `fn` tInteger)
        gcd_pat1  = [PVar "a", PLit (LitInt 0)]
        gcd_body1 = Var "a"
        gcd_pat2  = [PVar "a", PVar "b"]
        gcd_body2 = ap [Var "gcd", Var "b", ap [Var "mod", Var "a", Var "b"]]
        
        mod_type  = toScheme (tInteger `fn` tInteger `fn` tInteger)

        cons_type = Forall [Star] ([] :=> (TGen 0 `fn` list (TGen 0) `fn` list (TGen 0)))
        nil_type  = Forall [Star] ([] :=> (list (TGen 0)))

        foldr_type  = Forall [Star, Star] ([] :=> ((TGen 0 `fn` TGen 1 `fn` TGen 1) `fn` TGen 1 `fn` list (TGen 0) `fn` TGen 1))
        foldr_pat1  = [PWildcard, PVar "a", PCon ("[]":>:nil_type) []]
        foldr_body1 = Var "a"
        foldr_pat2  = [PVar "f", PVar "a", PAs "xxs" (PCon (":":>:cons_type) [PVar "x", PVar "xs"]  )]
        foldr_body2 = ap [Var "f", Var "x", ap [Var "foldr", Var "f", Var "a", Var "xs"]]

        and_type = toScheme (list tBool `fn` tBool)
        and_pat  = []
        and_body = ap [Var "foldr", Var "(&&)", Const ("True" :>: toScheme tBool)]


        alternating_type = toScheme (tInteger `fn` list tInteger)
        alternating_pat  = [PVar "x"]
        alternating1_body = ap [Const ("(:)":>:cons_type), Var "x", ap [Var "g", Var "x"]]
        alternating2_body = ap [Const ("(:)":>:cons_type), Var "x", ap [Var "f", ap [Var "decr", Var "x"]]]

        mutualex_type = Forall [Star] ([IsIn "Eq"  (TGen 0)] :=> (TGen 0 `fn` tBool))
        mutualex_pat  = [PVar "x"]
        mutualex_body = ap [Var "(||)", ap [Var "(==)", Var "x", Var "x"], ap [Var "g", Const ("True" :>: toScheme tBool)]]
        mutualim_type = Forall [Star] ([IsIn "Ord" (TGen 0)] :=> (TGen 0 `fn` tBool))
        mutualim_pat  = [PVar "y"]
        mutualim_body = ap [Var "(||)", ap [Var "(<=)", Var "y", Var "y"], ap [Var "f", Const ("True" :>: toScheme tBool)]]

        leq_type  = Forall [Star] ([IsIn "Ord" (TGen 0)] :=> (TGen 0 `fn` TGen 0 `fn` tBool))
        eq_type   = Forall [Star] ([IsIn "Eq"  (TGen 0)] :=> (TGen 0 `fn` TGen 0 `fn` tBool))
        land_type = toScheme (tBool `fn` tBool `fn` tBool)
        lor_type  = toScheme (tBool `fn` tBool `fn` tBool)
        fromIntegral_type = Forall [Star, Star] ([IsIn "Integral" (TGen 0), IsIn "Num" (TGen 1)] :=> (TGen 0 `fn` TGen 1))
        integralAdd_type = Forall [Star] ([IsIn "Num" (TGen 0)] :=> (TGen 0 `fn` TGen 0 `fn` TGen 0))

        fulting_type = Forall [] ([] :=> (tInt `fn` tBool))
        fulting_pat  = [PVar "v"]
        fulting_body = ap [Var "(==)", Var "v", ap [Var "fromIntegral", ap [Var "(+)", Lit (LitIntegral 2), Lit (LitIntegral 3)]]]

          --        xxx :: Int -> Bool
          --xxx v = v == (fromIntegral (2 + 3))

    it "typechecks `const`" $ do
      let def = ([( "const", const_type , [( const_pat , const_body )] )] , [])
      tiProgram initialEnv [] [def] `shouldContain` ["const" :>: const_type]
    

    it "infers type for `const`" $ do
      let def = ([] , [[( "const", [(const_pat , const_body)] )]] )
      tiProgram initialEnv [] [def] `shouldContain` ["const" :>: const_type]
    

    it "typechecks `gcd`" $ do
      let def = ( [( "gcd", gcd_type , [ (gcd_pat1, gcd_body1)
                                       , (gcd_pat2, gcd_body2)
                                       ]
                  )] , [] )
      tiProgram initialEnv ["mod" :>: mod_type] [def] `shouldContain` ["gcd" :>: gcd_type]
    

    it "infers type for `gcd`" $ do
      let def = ( [] , [[( "gcd", [ (gcd_pat1, gcd_body1)
                                  , (gcd_pat2, gcd_body2)
                                  ]
                  )]] )
      tiProgram initialEnv ["mod" :>: mod_type] [def] `shouldContain` ["gcd" :>: gcd_type]
    

    it "typechecks `foldr`" $ do
      let def = ([("foldr", foldr_type, [(foldr_pat1, foldr_body1),(foldr_pat2,foldr_body2)])], [])
          res = tiProgram initialEnv [] [def]
      res `shouldContain` ["foldr" :>: foldr_type]
    

    it "infers type for `foldr`" $ do
      let def = ([], [[  ( "foldr", [ (foldr_pat1, foldr_body1)
                                    , (foldr_pat2, foldr_body2)])
                     ]]
                )
          res = tiProgram initialEnv [] [def]
      res `shouldContain` ["foldr" :>: foldr_type]
    

    it "typechecks `and`" $ do
      let def = ([("and", and_type, [(and_pat, and_body)])], [])
          res = tiProgram initialEnv ["foldr" :>: foldr_type, "(&&)" :>: land_type] [def]
      res `shouldContain` ["and" :>: and_type]
    

    it "infers type for `and`" $ do
      let def = ([], [[("and", [(and_pat, and_body)])]])
          res = tiProgram initialEnv ["foldr" :>: foldr_type, "(&&)" :>: land_type] [def]
      res `shouldContain` ["and" :>: and_type]
    

    it "typechecks `foldr` & infers `and`" $ do
      let defs = [ ([], [[  ( "foldr", [ (foldr_pat1, foldr_body1)
                                       , (foldr_pat2, foldr_body2)])
                        ]]
                   )
                 , ([], [[  ( "and", [(and_pat, and_body)])
                        ]]
                   )
                 ]
          res = tiProgram initialEnv ["(&&)" :>: land_type] defs
      res `shouldContain` ["foldr" :>: foldr_type]
      res `shouldContain` ["and" :>: and_type]
    

    it "typechecks mutually-recursive functions" $ do
      let def = ( [ ("f", alternating_type, [(alternating_pat, alternating1_body)])
                  , ("g", alternating_type, [(alternating_pat, alternating2_body)])
                  ]
                , [])
          res = tiProgram initialEnv ["decr" :>: toScheme (tInteger `fn` tInteger)] [def]
      res `shouldContain` ["f" :>: alternating_type]
      res `shouldContain` ["g" :>: alternating_type]


    it "infers types for mutually-recursive functions" $ do
      let def = ( [], [[ ("f", [(alternating_pat, alternating1_body)])
                       , ("g", [(alternating_pat, alternating2_body)])
                      ]]
                )
          res = tiProgram initialEnv ["decr" :>: toScheme (tInteger `fn` tInteger)] [def]
      res `shouldContain` ["f" :>: alternating_type]
      res `shouldContain` ["g" :>: alternating_type]


    it "typechecks the example with mutually-recursive functions, one typed, both using typeclasses" $ do
      let def = ( [(  "f", mutualex_type, [(mutualex_pat, mutualex_body)])]
                , [[( "g",                [(mutualim_pat, mutualim_body)] )]])
          classenvT = addClass "Eq" []
                  <:> addClass "Ord" ["Eq"]
                  <:> addInst [] (IsIn "Eq" tBool)
                  <:> addInst [] (IsIn "Ord" tBool)
          Just classenv = classenvT initialEnv
          res = tiProgram classenv ["(<=)":>:leq_type, "(==)":>:eq_type, "(||)":>:lor_type] [def]
      res `shouldContain` ["f" :>: mutualex_type]
      res `shouldContain` ["g" :>: mutualim_type]


    it "resolves ambiguities: `fromIntegral (2 + 3)`" $ do
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


  describe "coverage booster" $ do -- nasty hack to ignore some debugging stuff in analysis of coverage
    it "should ignore Show instances" $ do
      length (concatMap show [LitChar 'c', LitFloat 1.0, LitInt (1 :: Integer), LitIntegral 1, LitStr "lel"]) `shouldSatisfy` (>0)
      length (concatMap show [Star, Kfun Star Star]) `shouldSatisfy` (>0)
      length (          show [Star, Kfun (Kfun Star Star) (Kfun (Kfun Star Star) (Kfun Star Star))]) `shouldSatisfy` (>0)
