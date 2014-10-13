module Test.Luna.Typechecker.AmbiguitySpec (spec) where


import Luna.Typechecker.Ambiguity
import Luna.Typechecker.Assumptions
import Luna.Typechecker.BindingGroups
import Luna.Typechecker.Typeclasses

import Luna.Typechecker.AST.ClassID
import Luna.Typechecker.AST.Kind
import Luna.Typechecker.AST.Lit
import Luna.Typechecker.AST.Pat
import Luna.Typechecker.AST.Scheme
import Luna.Typechecker.AST.TID
import Luna.Typechecker.AST.Type
import Luna.Typechecker.AST.VarID

import Luna.Typechecker

import Luna.Typechecker.Internal.Logger

import Data.Either                      (isLeft,isRight)

import Test.Hspec

import Test.Luna.Typechecker.Common


spec :: Spec
spec = do
  let fulting_type = Forall [] ([] :=> (tInt `fn` tBool))
      fulting_pat  = [PVar (VarID "v")]
      fulting_body = ap [Var (VarID "(==)"), Var (VarID "v"), ap [Var (VarID "fromIntegral"), ap [Var (VarID "(+)"), Lit (LitIntegral 2), Lit (LitIntegral 3)]]]

      fulting2_type = Forall [] ([] :=> (tInt `fn` tBool))
      fulting2_pat  = [PVar (VarID "v")]
      fulting2_body = ap [Var (VarID "(==)"), Var (VarID "v"), ap [Var (VarID "fromMytype"), ap [Var (VarID "xx"), Var (VarID "my1"), Var (VarID "my2")]]]

      fultingdef1 = ( [( VarID "fulting_type",  fulting_type,  [(fulting_pat, fulting_body)])]   , [])
      fultingdef2 = ( [( VarID "fulting2_type", fulting2_type, [(fulting2_pat, fulting2_body)])] , [])

      myType1 = TCon (Tycon (TID "MyType1") Star) :: Type
      myType2 = TCon (Tycon (TID "MyType2") Star) :: Type
      mys     = Forall [Star] ([IsIn (ClassID "MyType") (TGen 0)] :=> TGen 0)

      xx_type = Forall [Star] ([IsIn (ClassID "MyType") (TGen 0)] :=> (TGen 0 `fn` TGen 0 `fn` TGen 0))

  describe "defaulting" $ do
    it "solves ambiguity for: `fromIntegral (2 + 3)`" $ do
      let classenvT = addClass (ClassID "Eq") []
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
          Right classenv = evalLogger $ classenvT initialEnv
          (eres, _) = tiProgram classenv [eqBG^.asmp, fromIntegralBG^.asmp, integralAddBG^.asmp] [fultingdef1]
      eres `shouldSatisfy` isRight
      let Right res = eres
      res `shouldContain` [VarID "fulting_type" :>: fulting_type]

    it "can't solve ambiguity for: `fromMytype (xx my1 my2)` (I)" $ do
      let classenvT = addClass (ClassID "Eq")     []
                  <:> addClass (ClassID "MyType") [ClassID "Eq"]
                  <:> addInst [] (IsIn (ClassID "Eq")     tInt)
                  <:> addInst [] (IsIn (ClassID "Eq")     myType1) <:> addInst [] (IsIn (ClassID "Eq")     myType2)
                  <:> addInst [] (IsIn (ClassID "MyType") myType1) <:> addInst [] (IsIn (ClassID "MyType") myType2)

          fromMytype_type = Forall [Star, Star] ([IsIn (ClassID "MyType") (TGen 0)] :=> (TGen 0 `fn` tInt))

          Right classenv = evalLogger $ classenvT initialEnv
          (eres, _) = tiProgram classenv [eqBG^.asmp, VarID "fromMytype" :>:fromMytype_type, VarID "xx" :>:xx_type, VarID "my1" :>:mys, VarID "my2" :>:mys] [fultingdef2]
      eres `shouldSatisfy` isLeft

    it "can't solve ambiguity for: `fromMytype (xx my1 my2)` (II)" $ do
      let classenvT = addClass (ClassID "Eq")     []
                  <:> addClass (ClassID "MyType") [ClassID "Eq"]
                  <:> addInst [] (IsIn (ClassID "Eq")     tInt)
                  <:> addInst [] (IsIn (ClassID "Eq")     myType1) <:> addInst [] (IsIn (ClassID "Eq")     myType2)
                  <:> addInst [] (IsIn (ClassID "MyType") myType1) <:> addInst [] (IsIn (ClassID "MyType") myType2)

          fromMytype_type = Forall [] ([IsIn (ClassID "MyType") (TVar $ Tyvar (TID "a") Star)] :=> ((TVar $ Tyvar (TID "a") Star) `fn` tInt))

          Right classenv = evalLogger $ classenvT initialEnv
          (eres, _) = tiProgram classenv [eqBG^.asmp, VarID "fromMytype" :>:fromMytype_type, VarID "xx" :>:xx_type, VarID "my1" :>:mys, VarID "my2" :>:mys] [fultingdef2]
      eres `shouldSatisfy` isLeft

    it "can't solve ambiguity for: `fromMytype (xx my1 my2)` (III)" $ do
      let classenvT = addClass (ClassID "MyEq")     []
                  <:> addClass (ClassID "MyType") [ClassID "MyEq"]
                  <:> addInst [] (IsIn (ClassID "MyEq")     tInt)
                  <:> addInst [] (IsIn (ClassID "MyEq")     myType1) <:> addInst [] (IsIn (ClassID "MyEq")     myType2)
                  <:> addInst [] (IsIn (ClassID "MyType") myType1) <:> addInst [] (IsIn (ClassID "MyType") myType2)

          myeq_type = Forall [Star] ([IsIn (ClassID "MyEq") (TGen 0)] :=> (TGen 0 `fn` TGen 0 `fn` tBool))

          fromMytype_type = Forall [] ([IsIn (ClassID "MyType") (TVar $ Tyvar (TID "a") Star)] :=> ((TVar $ Tyvar (TID "a") Star) `fn` tInt))

          Right classenv = evalLogger $ classenvT initialEnv
          (eres, _) = tiProgram classenv [VarID "(==)" :>:myeq_type, VarID "fromMytype" :>:fromMytype_type, VarID "xx" :>:xx_type, VarID "my1" :>:mys, VarID "my2" :>:mys] [fultingdef2]
      eres `shouldSatisfy` isLeft

  describe "candidates" $
    it "works" $ do
      let Right ce = evalLogger ((  addClass (ClassID "Eq")       []
                         <:> addClass (ClassID "Ord")      [ClassID "Eq"]
                         <:> addClass (ClassID "Num")      []
                         <:> addClass (ClassID "Real")     [ClassID "Num", ClassID "Ord"]
                         <:> addClass (ClassID "Enum")     []
                         <:> addClass (ClassID "Integral") [ClassID "Real", ClassID "Enum"]
                         <:> addClass (ClassID "Functor")     []
                         <:> addClass (ClassID "Applicative") [ClassID "Functor"]
                         <:> addClass (ClassID "LOLOLOL")  []
     
                         <:> addInst [IsIn (ClassID "Functor") (TVar $ Tyvar (TID "f") Star), IsIn (ClassID "Ord") (TVar $ Tyvar (TID "a") Star)]
                                     (IsIn (ClassID "Ord") (TAp (TVar $ Tyvar (TID "f") Star) (TVar $ Tyvar (TID "a") Star)))
                         <:> addInst [] (IsIn (ClassID "Eq")       tInt)   <:> addInst [] (IsIn (ClassID "Eq")       tInteger)
                         <:> addInst [] (IsIn (ClassID "Ord")      tInt)   <:> addInst [] (IsIn (ClassID "Ord")      tInteger)
                         <:> addInst [] (IsIn (ClassID "Num")      tInt)   <:> addInst [] (IsIn (ClassID "Num")      tInteger)
                         <:> addInst [] (IsIn (ClassID "Real")     tInt)   <:> addInst [] (IsIn (ClassID "Real")     tInteger)
                         <:> addInst [] (IsIn (ClassID "Enum")     tInt)   <:> addInst [] (IsIn (ClassID "Enum")     tInteger)
                         <:> addInst [] (IsIn (ClassID "Integral") tInt)   <:> addInst [] (IsIn (ClassID "Integral") tInteger)
                         <:> addInst [] (IsIn (ClassID "LOLOLOL")  tInt)   <:> addInst [] (IsIn (ClassID "LOLOLOL")  tInteger)
                         <:> addInst [] (IsIn (ClassID "Functor")     tList)
                         <:> addInst [] (IsIn (ClassID "Applicative") tList)
                          ) initialEnv)
          f = TVar $ Tyvar (TID "f") Star
          a = TVar $ Tyvar (TID "a") Star
      evalLogger (candidates ce (Tyvar (TID "a") Star, [IsIn (ClassID "Ord") (TAp f a)])               ) `shouldBe` Right []
      evalLogger (candidates ce (Tyvar (TID "a") Star, [IsIn (ClassID "Integral") a])                  ) `shouldBe` Right [tInteger]
      evalLogger (candidates ce (Tyvar (TID "a") Star, [IsIn (ClassID "Integral") a, IsIn (ClassID "LOLOLOL") a])) `shouldBe` Right []


  describe "fighting monomorphism restriction" $
    it "works" $ do
      let as = [ integralAddBG  ^.asmp
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
          bgs = [( []
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
                )]

          Right ce = evalLogger ((  addClass (ClassID "Eq") []
                             <:> addClass (ClassID "Ord") [ClassID "Eq"]
                             <:> addClass (ClassID "Num") []
                             <:> addClass (ClassID "Real") [ClassID "Num", ClassID "Ord"]
                             <:> addClass (ClassID "Enum") []
                             <:> addClass (ClassID "Integral") [ClassID "Real", ClassID "Enum"]
                             <:> addClass (ClassID "Functor") []
                             <:> addInst [] (IsIn (ClassID "Eq") tInt)       <:> addInst [] (IsIn (ClassID "Eq") tInteger)
                             <:> addInst [] (IsIn (ClassID "Ord") tInt)      <:> addInst [] (IsIn (ClassID "Ord") tInteger)
                             <:> addInst [] (IsIn (ClassID "Num") tInt)      <:> addInst [] (IsIn (ClassID "Num") tInteger)
                             <:> addInst [] (IsIn (ClassID "Real") tInt)     <:> addInst [] (IsIn (ClassID "Real") tInteger)
                             <:> addInst [] (IsIn (ClassID "Enum") tInt)     <:> addInst [] (IsIn (ClassID "Enum") tInteger)
                             <:> addInst [] (IsIn (ClassID "Integral") tInt) <:> addInst [] (IsIn (ClassID "Integral") tInteger)
                             <:> addInst [] (IsIn (ClassID "Functor") tList)
                             <:> addInst [IsIn (ClassID "Functor") (TVar $ Tyvar (TID "f") Star), IsIn (ClassID "Ord") (TVar $ Tyvar (TID "a") Star)]
                                         (IsIn (ClassID "Ord") (TAp (TVar $ Tyvar (TID "f") Star) (TVar $ Tyvar (TID "a") Star)))
                              ) initialEnv)
          (eres, _) = tiProgram ce as bgs
      eres `shouldSatisfy` isRight
      let Right res = eres
      res `shouldContain` [VarID "pie" :>:toScheme tInteger]
