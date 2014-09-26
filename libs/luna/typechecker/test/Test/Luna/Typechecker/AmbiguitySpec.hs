module Test.Luna.Typechecker.AmbiguitySpec (spec) where


import Luna.Typechecker.Ambiguity
import Luna.Typechecker.Assumptions
import Luna.Typechecker.BindingGroups
import Luna.Typechecker.Typeclasses

import Luna.Typechecker.AST.Kind
import Luna.Typechecker.AST.Lit
import Luna.Typechecker.AST.Pat
import Luna.Typechecker.AST.Scheme
import Luna.Typechecker.AST.Type

import Luna.Typechecker

import Luna.Typechecker.Internal.Logger

import Data.Either                      (isLeft,isRight)

import Test.Hspec

import Test.Luna.Typechecker.Common


spec :: Spec
spec = do
  let fulting_type = Forall [] ([] :=> (tInt `fn` tBool))
      fulting_pat  = [PVar "v"]
      fulting_body = ap [Var "(==)", Var "v", ap [Var "fromIntegral", ap [Var "(+)", Lit (LitIntegral 2), Lit (LitIntegral 3)]]]

      fulting2_type = Forall [] ([] :=> (tInt `fn` tBool))
      fulting2_pat  = [PVar "v"]
      fulting2_body = ap [Var "(==)", Var "v", ap [Var "fromMytype", ap [Var "xx", Var "my1", Var "my2"]]]

      fultingdef1 = ( [( "fulting_type",  fulting_type,  [(fulting_pat, fulting_body)])]   , [])
      fultingdef2 = ( [( "fulting2_type", fulting2_type, [(fulting2_pat, fulting2_body)])] , [])

      myType1 = TCon (Tycon "MyType1" Star) :: Type
      myType2 = TCon (Tycon "MyType2" Star) :: Type
      mys     = Forall [Star] ([IsIn "MyType" (TGen 0)] :=> TGen 0)

      xx_type = Forall [Star] ([IsIn "MyType" (TGen 0)] :=> (TGen 0 `fn` TGen 0 `fn` TGen 0))

  describe "defaulting" $ do
    it "solves ambiguity for: `fromIntegral (2 + 3)`" $ do
      let classenvT = addClass "Eq" []
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
          Right classenv = evalLogger $ classenvT initialEnv
          (eres, _) = tiProgram classenv [eqBG^.asmp, fromIntegralBG^.asmp, integralAddBG^.asmp] [fultingdef1]
      eres `shouldSatisfy` isRight
      let Right res = eres
      res `shouldContain` ["fulting_type" :>: fulting_type]

    it "can't solve ambiguity for: `fromMytype (xx my1 my2)` (I)" $ do
      let classenvT = addClass "Eq"     []
                  <:> addClass "MyType" ["Eq"]
                  <:> addInst [] (IsIn "Eq"     tInt)
                  <:> addInst [] (IsIn "Eq"     myType1) <:> addInst [] (IsIn "Eq"     myType2)
                  <:> addInst [] (IsIn "MyType" myType1) <:> addInst [] (IsIn "MyType" myType2)

          fromMytype_type = Forall [Star, Star] ([IsIn "MyType" (TGen 0)] :=> (TGen 0 `fn` tInt))

          Right classenv = evalLogger $ classenvT initialEnv
          (eres, _) = tiProgram classenv [eqBG^.asmp, "fromMytype":>:fromMytype_type, "xx":>:xx_type, "my1":>:mys, "my2":>:mys] [fultingdef2]
      eres `shouldSatisfy` isLeft

    it "can't solve ambiguity for: `fromMytype (xx my1 my2)` (II)" $ do
      let classenvT = addClass "Eq"     []
                  <:> addClass "MyType" ["Eq"]
                  <:> addInst [] (IsIn "Eq"     tInt)
                  <:> addInst [] (IsIn "Eq"     myType1) <:> addInst [] (IsIn "Eq"     myType2)
                  <:> addInst [] (IsIn "MyType" myType1) <:> addInst [] (IsIn "MyType" myType2)

          fromMytype_type = Forall [] ([IsIn "MyType" (TVar $ Tyvar "a" Star)] :=> ((TVar $ Tyvar "a" Star) `fn` tInt))

          Right classenv = evalLogger $ classenvT initialEnv
          (eres, _) = tiProgram classenv [eqBG^.asmp, "fromMytype":>:fromMytype_type, "xx":>:xx_type, "my1":>:mys, "my2":>:mys] [fultingdef2]
      eres `shouldSatisfy` isLeft

    it "can't solve ambiguity for: `fromMytype (xx my1 my2)` (III)" $ do
      let classenvT = addClass "MyEq"     []
                  <:> addClass "MyType" ["MyEq"]
                  <:> addInst [] (IsIn "MyEq"     tInt)
                  <:> addInst [] (IsIn "MyEq"     myType1) <:> addInst [] (IsIn "MyEq"     myType2)
                  <:> addInst [] (IsIn "MyType" myType1) <:> addInst [] (IsIn "MyType" myType2)

          myeq_type = Forall [Star] ([IsIn "MyEq" (TGen 0)] :=> (TGen 0 `fn` TGen 0 `fn` tBool))

          fromMytype_type = Forall [] ([IsIn "MyType" (TVar $ Tyvar "a" Star)] :=> ((TVar $ Tyvar "a" Star) `fn` tInt))

          Right classenv = evalLogger $ classenvT initialEnv
          (eres, _) = tiProgram classenv ["(==)":>:myeq_type, "fromMytype":>:fromMytype_type, "xx":>:xx_type, "my1":>:mys, "my2":>:mys] [fultingdef2]
      eres `shouldSatisfy` isLeft

  describe "candidates" $
    it "works" $ do
      let Right ce = evalLogger ((  addClass "Eq"       []
                         <:> addClass "Ord"      ["Eq"]
                         <:> addClass "Num"      []
                         <:> addClass "Real"     ["Num", "Ord"]
                         <:> addClass "Enum"     []
                         <:> addClass "Integral" ["Real", "Enum"]
                         <:> addClass "Functor"     []
                         <:> addClass "Applicative" ["Functor"]
                         <:> addClass "LOLOLOL"  []
     
                         <:> addInst [IsIn "Functor" (TVar $ Tyvar "f" Star), IsIn "Ord" (TVar $ Tyvar "a" Star)]
                                     (IsIn "Ord" (TAp (TVar $ Tyvar "f" Star) (TVar $ Tyvar "a" Star)))
                         <:> addInst [] (IsIn "Eq"       tInt)   <:> addInst [] (IsIn "Eq"       tInteger)
                         <:> addInst [] (IsIn "Ord"      tInt)   <:> addInst [] (IsIn "Ord"      tInteger)
                         <:> addInst [] (IsIn "Num"      tInt)   <:> addInst [] (IsIn "Num"      tInteger)
                         <:> addInst [] (IsIn "Real"     tInt)   <:> addInst [] (IsIn "Real"     tInteger)
                         <:> addInst [] (IsIn "Enum"     tInt)   <:> addInst [] (IsIn "Enum"     tInteger)
                         <:> addInst [] (IsIn "Integral" tInt)   <:> addInst [] (IsIn "Integral" tInteger)
                         <:> addInst [] (IsIn "LOLOLOL"  tInt)   <:> addInst [] (IsIn "LOLOLOL"  tInteger)
                         <:> addInst [] (IsIn "Functor"     tList)
                         <:> addInst [] (IsIn "Applicative" tList)
                          ) initialEnv)
          f = TVar $ Tyvar "f" Star
          a = TVar $ Tyvar "a" Star
      evalLogger (candidates ce (Tyvar "a" Star, [IsIn "Ord" (TAp f a)])               ) `shouldBe` Right []
      evalLogger (candidates ce (Tyvar "a" Star, [IsIn "Integral" a])                  ) `shouldBe` Right [tInteger]
      evalLogger (candidates ce (Tyvar "a" Star, [IsIn "Integral" a, IsIn "LOLOLOL" a])) `shouldBe` Right []


  describe "fighting monomorphism restriction" $
    it "works" $ do
      let as = [ integralAddBG   ^. asmp
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

          Right ce = evalLogger ((  addClass "Eq" []
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
                                         (IsIn "Ord" (TAp (TVar $ Tyvar "f" Star) (TVar $ Tyvar "a" Star)))
                              ) initialEnv)
          (eres, _) = tiProgram ce as bgs
      eres `shouldSatisfy` isRight
      let Right res = eres
      res `shouldContain` ["pie":>:toScheme tInteger]
