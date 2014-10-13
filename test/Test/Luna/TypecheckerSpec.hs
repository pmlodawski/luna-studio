{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}

module Test.Luna.TypecheckerSpec (spec) where


import Luna.Typechecker.AST
import Luna.Typechecker.Type

import Logger

import Test.Hspec



type TCLoggerT = LoggerT String

class Typechecker a b | a -> b where
  typechecker :: (Monad m) => String -> a -> TCLoggerT m b



spec :: Spec
spec =
  describe "basic typechecking" $
    it "typechecks literals" $
      True
      --typechecker "Main.luna" (LitInt 10)        `shouldBe` Tycon (TyID "Int")
      --typechecker "Main.luna" (ELit (LitInt 10)) `shouldBe` Tycon (TyID "Int")