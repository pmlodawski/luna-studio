{-# LANGUAGE ScopedTypeVariables #-}

module TypecheckerInternalsSpec (main, spec) where


import           Control.Monad                (liftM,liftM2)

import           Test.Hspec
import           Test.QuickCheck

import           Flowbox.Typechecker.Basic

import           Flowbox.Luna.Data.AST.Common (ID)
import           Flowbox.Luna.Data.AST.Expr   (Expr(..))
import           Flowbox.Luna.Data.AST.Lit    (Lit(..))
import qualified Flowbox.Luna.Data.AST.Lit    as L

main :: IO ()
main = hspec spec

instance Arbitrary Expr where
  arbitrary = oneof ([expr_NOP, expr_Lit] :: [Gen Expr])

-- expr_Accessor     = liftM? Accessor     arbitrary
-- expr_App          = liftM? App          arbitrary
-- expr_AppCons_     = liftM? AppCons_     arbitrary
-- expr_Arg          = liftM? Arg          arbitrary
-- expr_Assignment   = liftM? Assignment   arbitrary
-- expr_Case         = liftM? Case         arbitrary
-- expr_Con          = liftM? Con          arbitrary
-- expr_Cond         = liftM? Cond         arbitrary
-- expr_ConD         = liftM? ConD         arbitrary
-- expr_Data         = liftM? Data         arbitrary
-- expr_Field        = liftM? Field        arbitrary
-- expr_Function     = liftM? Function     arbitrary
-- expr_Grouped      = liftM? Grouped      arbitrary
-- expr_Import       = liftM? Import       arbitrary
-- expr_ImportNative = liftM? ImportNative arbitrary
-- expr_Infix        = liftM? Infix        arbitrary
-- expr_Lambda       = liftM? Lambda       arbitrary
-- expr_List         = liftM? List         arbitrary
expr_Lit          = liftM2 Lit          arbitrary arbitrary
-- expr_Match        = liftM? Match        arbitrary
-- expr_Native       = liftM? Native       arbitrary
-- expr_NativeCode   = liftM? NativeCode   arbitrary
-- expr_NativeVar    = liftM? NativeVar    arbitrary
expr_NOP          = liftM  NOP          arbitrary
-- expr_RangeFrom    = liftM? RangeFrom    arbitrary
-- expr_RangeFromTo  = liftM? RangeFromTo  arbitrary
-- expr_RecordUpdate = liftM? RecordUpdate arbitrary
-- expr_Ref          = liftM? Ref          arbitrary
-- expr_RefType      = liftM? RefType      arbitrary
-- expr_Tuple        = liftM? Tuple        arbitrary
-- expr_TypeAlias    = liftM? TypeAlias    arbitrary
-- expr_Typed        = liftM? Typed        arbitrary
-- expr_TypeDef      = liftM? TypeDef      arbitrary
-- expr_Var          = liftM? Var          arbitrary
-- expr_Wildcard     = liftM? Wildcard     arbitrary

instance Arbitrary Lit where
    arbitrary = oneof ([lit_char, lit_string, lit_integer, lit_float] :: [Gen L.Lit])

lit_char    = liftM2 L.Char    arbitrary arbitrary
lit_float   = liftM2 L.Float   arbitrary arbitrary
lit_integer = liftM2 L.Integer arbitrary arbitrary
lit_string  = liftM2 L.String  arbitrary arbitrary


spec :: Spec
spec = do
  describe "black-box specification" $ do
    it "empty substitution does not change the value of Expr" $
      property $ \(expr::Expr) -> apply nullSubst expr `shouldBe` expr
    it "empty substitution does not change the value of Lit" $
      property $ \(lit::Lit)   -> apply nullSubst lit  `shouldBe` lit
    it "some ASTs have no type variables" $
      pending
    it "some ASTs have one type variable" $
      pending
    it "modifying the AST with >0 type variables modifies the type" $
      pending
