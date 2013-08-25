---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------

module Parser.ParserSpec where

import Test.Hspec
import Data.Either.Utils (forceEither)
import qualified Flowbox.Luna.Parser.Parser as Parser
import Flowbox.Luna.Parser.AST.AST -- for tests
import Flowbox.Luna.Parser.AST.Constant -- for tests

absolute x = x

main :: IO ()
main = hspec spec

parse s = forceEither $ Parser.parse $ unlines s

spec :: Spec
spec = do
  describe "entities" $ do
    describe "basic" $ do
      it "empty input"                        $ parse [""]              `shouldBe` []
      it "ident"                              $ parse ["ala"]           `shouldBe` [Identifier "ala"]
    describe "tuples" $ do
      it "empty tuple"                        $ parse ["()"]            `shouldBe` [Tuple {items = []}]
      it "single tuple"                       $ parse ["(a,)"]          `shouldBe` [Tuple {items = [Identifier "a"]}]
      it "tuple"                              $ parse ["(a,b,)"]        `shouldBe` [Tuple {items = [Identifier "a",Identifier "b"]}]

  describe "expressions" $ do
    describe "basic" $ do
      it "assignment"                         $ parse ["a=b"]           `shouldBe` [Assignment {src = Identifier "a", dst = Identifier "b"}]
      it "function call"                      $ parse ["a b c"]         `shouldBe` [Call {src = Identifier "a", args = [Identifier "b",Identifier "c"]}]

    describe "functions" $ do
      it "empty function"                     $ parse ["def f():"]      `shouldBe` [Function {name = "f", signature = [], body = []}]
      it "empty function with signature"      $ parse ["def f(x,y,z):"] `shouldBe` [Function {name = "f", signature = [Identifier "x",Identifier "y",Identifier "z"], body = []}]
      it "simple function"                    $ parse ["def f(x):x"]    `shouldBe` [Function {name = "f", signature = [Identifier "x"], body = [Identifier "x"]}]

