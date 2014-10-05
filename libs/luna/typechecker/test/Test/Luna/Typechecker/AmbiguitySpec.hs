module Test.Luna.Typechecker.AmbiguitySpec (spec) where


import Luna.Typechecker.Ambiguity
import Luna.Typechecker.Assumptions
import Luna.Typechecker.BindingGroups
import Luna.Typechecker.Typeclasses

import Luna.Typechecker.AST.Kind
import Luna.Typechecker.AST.Lit
import Luna.Typechecker.AST.Pat
import Luna.Typechecker.AST.Scheme
import Luna.Typechecker.AST.TID
import Luna.Typechecker.AST.Type

import Luna.Typechecker

import Luna.Typechecker.Internal.Logger

import Data.Either                      (isLeft,isRight)

import Test.Hspec

import Test.Luna.Typechecker.Common


spec :: Spec
spec = do
  let fulting_type = Forall [] ([] :=> (tInt `fn` tBool))
      fulting_pat  = [PVar (TID "v")]
      fulting_body = ap [Var (TID "(==)"), Var (TID "v"), ap [Var (TID "fromIntegral"), ap [Var (TID "(+)"), Lit (LitIntegral 2), Lit (LitIntegral 3)]]]

      fulting2_type = Forall [] ([] :=> (tInt `fn` tBool))
      fulting2_pat  = [PVar (TID "v")]
      fulting2_body = ap [Var (TID "(==)"), Var (TID "v"), ap [Var (TID "fromMytype"), ap [Var (TID "xx"), Var (TID "my1"), Var (TID "my2")]]]

      fultingdef1 = ( [( TID "fulting_type",  fulting_type,  [(fulting_pat, fulting_body)])]   , [])
      fultingdef2 = ( [( TID "fulting2_type", fulting2_type, [(fulting2_pat, fulting2_body)])] , [])

      myType1 = TCon (Tycon (TID "MyType1") Star) :: Type
      myType2 = TCon (Tycon (TID "MyType2") Star) :: Type
      mys     = Forall [Star] ([IsIn (TID "MyType") (TGen 0)] :=> TGen 0)

      xx_type = Forall [Star] ([IsIn (TID "MyType") (TGen 0)] :=> (TGen 0 `fn` TGen 0 `fn` TGen 0))

  describe "defaulting" $ do
    it "solves ambiguity for: `fromIntegral (2 + 3)`" $ do
      let classenvT = addClass (TID "Eq") []
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
          Right classenv = evalLogger $ classenvT initialEnv
          (eres, _) = tiProgram classenv [eqBG^.asmp, fromIntegralBG^.asmp, integralAddBG^.asmp] [fultingdef1]
      eres `shouldSatisfy` isRight
      let Right res = eres
      res `shouldContain` [TID "fulting_type" :>: fulting_type]

    it "can't solve ambiguity for: `fromMytype (xx my1 my2)` (I)" $ do
      let classenvT = addClass (TID "Eq")     []
                  <:> addClass (TID "MyType") [TID "Eq"]
                  <:> addInst [] (IsIn (TID "Eq")     tInt)
                  <:> addInst [] (IsIn (TID "Eq")     myType1) <:> addInst [] (IsIn (TID "Eq")     myType2)
                  <:> addInst [] (IsIn (TID "MyType") myType1) <:> addInst [] (IsIn (TID "MyType") myType2)

          fromMytype_type = Forall [Star, Star] ([IsIn (TID "MyType") (TGen 0)] :=> (TGen 0 `fn` tInt))

          Right classenv = evalLogger $ classenvT initialEnv
          (eres, _) = tiProgram classenv [eqBG^.asmp, TID "fromMytype" :>:fromMytype_type, TID "xx" :>:xx_type, TID "my1" :>:mys, TID "my2" :>:mys] [fultingdef2]
      eres `shouldSatisfy` isLeft

    it "can't solve ambiguity for: `fromMytype (xx my1 my2)` (II)" $ do
      let classenvT = addClass (TID "Eq")     []
                  <:> addClass (TID "MyType") [TID "Eq"]
                  <:> addInst [] (IsIn (TID "Eq")     tInt)
                  <:> addInst [] (IsIn (TID "Eq")     myType1) <:> addInst [] (IsIn (TID "Eq")     myType2)
                  <:> addInst [] (IsIn (TID "MyType") myType1) <:> addInst [] (IsIn (TID "MyType") myType2)

          fromMytype_type = Forall [] ([IsIn (TID "MyType") (TVar $ Tyvar (TID "a") Star)] :=> ((TVar $ Tyvar (TID "a") Star) `fn` tInt))

          Right classenv = evalLogger $ classenvT initialEnv
          (eres, _) = tiProgram classenv [eqBG^.asmp, TID "fromMytype" :>:fromMytype_type, TID "xx" :>:xx_type, TID "my1" :>:mys, TID "my2" :>:mys] [fultingdef2]
      eres `shouldSatisfy` isLeft

    it "can't solve ambiguity for: `fromMytype (xx my1 my2)` (III)" $ do
      let classenvT = addClass (TID "MyEq")     []
                  <:> addClass (TID "MyType") [TID "MyEq"]
                  <:> addInst [] (IsIn (TID "MyEq")     tInt)
                  <:> addInst [] (IsIn (TID "MyEq")     myType1) <:> addInst [] (IsIn (TID "MyEq")     myType2)
                  <:> addInst [] (IsIn (TID "MyType") myType1) <:> addInst [] (IsIn (TID "MyType") myType2)

          myeq_type = Forall [Star] ([IsIn (TID "MyEq") (TGen 0)] :=> (TGen 0 `fn` TGen 0 `fn` tBool))

          fromMytype_type = Forall [] ([IsIn (TID "MyType") (TVar $ Tyvar (TID "a") Star)] :=> ((TVar $ Tyvar (TID "a") Star) `fn` tInt))

          Right classenv = evalLogger $ classenvT initialEnv
          (eres, _) = tiProgram classenv [TID "(==)" :>:myeq_type, TID "fromMytype" :>:fromMytype_type, TID "xx" :>:xx_type, TID "my1" :>:mys, TID "my2" :>:mys] [fultingdef2]
      eres `shouldSatisfy` isLeft

  describe "candidates" $
    it "works" $ do
      let Right ce = evalLogger ((  addClass (TID "Eq")       []
                         <:> addClass (TID "Ord")      [TID "Eq"]
                         <:> addClass (TID "Num")      []
                         <:> addClass (TID "Real")     [TID "Num", TID "Ord"]
                         <:> addClass (TID "Enum")     []
                         <:> addClass (TID "Integral") [TID "Real", TID "Enum"]
                         <:> addClass (TID "Functor")     []
                         <:> addClass (TID "Applicative") [TID "Functor"]
                         <:> addClass (TID "LOLOLOL")  []
     
                         <:> addInst [IsIn (TID "Functor") (TVar $ Tyvar (TID "f") Star), IsIn (TID "Ord") (TVar $ Tyvar (TID "a") Star)]
                                     (IsIn (TID "Ord") (TAp (TVar $ Tyvar (TID "f") Star) (TVar $ Tyvar (TID "a") Star)))
                         <:> addInst [] (IsIn (TID "Eq")       tInt)   <:> addInst [] (IsIn (TID "Eq")       tInteger)
                         <:> addInst [] (IsIn (TID "Ord")      tInt)   <:> addInst [] (IsIn (TID "Ord")      tInteger)
                         <:> addInst [] (IsIn (TID "Num")      tInt)   <:> addInst [] (IsIn (TID "Num")      tInteger)
                         <:> addInst [] (IsIn (TID "Real")     tInt)   <:> addInst [] (IsIn (TID "Real")     tInteger)
                         <:> addInst [] (IsIn (TID "Enum")     tInt)   <:> addInst [] (IsIn (TID "Enum")     tInteger)
                         <:> addInst [] (IsIn (TID "Integral") tInt)   <:> addInst [] (IsIn (TID "Integral") tInteger)
                         <:> addInst [] (IsIn (TID "LOLOLOL")  tInt)   <:> addInst [] (IsIn (TID "LOLOLOL")  tInteger)
                         <:> addInst [] (IsIn (TID "Functor")     tList)
                         <:> addInst [] (IsIn (TID "Applicative") tList)
                          ) initialEnv)
          f = TVar $ Tyvar (TID "f") Star
          a = TVar $ Tyvar (TID "a") Star
      evalLogger (candidates ce (Tyvar (TID "a") Star, [IsIn (TID "Ord") (TAp f a)])               ) `shouldBe` Right []
      evalLogger (candidates ce (Tyvar (TID "a") Star, [IsIn (TID "Integral") a])                  ) `shouldBe` Right [tInteger]
      evalLogger (candidates ce (Tyvar (TID "a") Star, [IsIn (TID "Integral") a, IsIn (TID "LOLOLOL") a])) `shouldBe` Right []


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
                )]

          Right ce = evalLogger ((  addClass (TID "Eq") []
                             <:> addClass (TID "Ord") [TID "Eq"]
                             <:> addClass (TID "Num") []
                             <:> addClass (TID "Real") [TID "Num", TID "Ord"]
                             <:> addClass (TID "Enum") []
                             <:> addClass (TID "Integral") [TID "Real", TID "Enum"]
                             <:> addClass (TID "Functor") []
                             <:> addInst [] (IsIn (TID "Eq") tInt)       <:> addInst [] (IsIn (TID "Eq") tInteger)
                             <:> addInst [] (IsIn (TID "Ord") tInt)      <:> addInst [] (IsIn (TID "Ord") tInteger)
                             <:> addInst [] (IsIn (TID "Num") tInt)      <:> addInst [] (IsIn (TID "Num") tInteger)
                             <:> addInst [] (IsIn (TID "Real") tInt)     <:> addInst [] (IsIn (TID "Real") tInteger)
                             <:> addInst [] (IsIn (TID "Enum") tInt)     <:> addInst [] (IsIn (TID "Enum") tInteger)
                             <:> addInst [] (IsIn (TID "Integral") tInt) <:> addInst [] (IsIn (TID "Integral") tInteger)
                             <:> addInst [] (IsIn (TID "Functor") tList)
                             <:> addInst [IsIn (TID "Functor") (TVar $ Tyvar (TID "f") Star), IsIn (TID "Ord") (TVar $ Tyvar (TID "a") Star)]
                                         (IsIn (TID "Ord") (TAp (TVar $ Tyvar (TID "f") Star) (TVar $ Tyvar (TID "a") Star)))
                              ) initialEnv)
          (eres, _) = tiProgram ce as bgs
      eres `shouldSatisfy` isRight
      let Right res = eres
      res `shouldContain` [TID "pie" :>:toScheme tInteger]
