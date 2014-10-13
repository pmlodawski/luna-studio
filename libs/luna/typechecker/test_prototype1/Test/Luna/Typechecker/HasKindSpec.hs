module Test.Luna.Typechecker.HasKindSpec (spec) where


import qualified Luna.Typechecker.AST.Type         as Ty

import           Luna.Typechecker.AST.Kind         (Kind(..))
import           Luna.Typechecker.HasKind          (kind)

import           Luna.Typechecker.Internal.Logger

import           Data.Either                       (isLeft)

import           Test.Hspec


spec :: Spec
spec =
  describe "class HasKind t" $ do
    describe "instance HasKind Tyvar" $
      it "kind :: t -> Kind" $ do
        evalLogger (kind (Ty.Tyvar undefined Star)            ) `shouldBe` Right Star
        evalLogger (kind (Ty.Tyvar undefined (Kfun Star Star))) `shouldBe` Right (Kfun Star Star)
    describe "instance HasKind Tycon" $
      it "kind :: t -> Kind" $ do
        evalLogger (kind (Ty.Tycon undefined Star)            ) `shouldBe` Right Star
        evalLogger (kind (Ty.Tycon undefined (Kfun Star Star))) `shouldBe` Right (Kfun Star Star)
    describe "instance HasKind Type" $
      it "kind :: t -> Kind" $ do
        evalLogger (kind Ty.tUnit                             ) `shouldBe` Right Star
        evalLogger (kind Ty.tChar                             ) `shouldBe` Right Star
        evalLogger (kind Ty.tInt                              ) `shouldBe` Right Star
        evalLogger (kind Ty.tInteger                          ) `shouldBe` Right Star
        evalLogger (kind Ty.tFloat                            ) `shouldBe` Right Star
        evalLogger (kind Ty.tDouble                           ) `shouldBe` Right Star

        evalLogger (kind Ty.tList                             ) `shouldBe` Right (Kfun Star Star)
        evalLogger (kind Ty.tArrow                            ) `shouldBe` Right (Kfun Star (Kfun Star Star))
        evalLogger (kind Ty.tTuple2                           ) `shouldBe` Right (Kfun Star (Kfun Star Star))

        evalLogger (kind Ty.tString                           ) `shouldBe` Right Star

        evalLogger (kind (Ty.TVar $ Ty.Tyvar undefined Star)  ) `shouldBe` Right Star
        evalLogger (kind (Ty.TCon $ Ty.Tycon undefined Star)  ) `shouldBe` Right Star
        evalLogger (kind (Ty.TAp Ty.tList Ty.tInt)            ) `shouldBe` Right Star
        evalLogger (kind (Ty.TAp Ty.tList Ty.tList))            `shouldSatisfy` isLeft
        evalLogger (kind (Ty.TGen undefined))                   `shouldSatisfy` isLeft
