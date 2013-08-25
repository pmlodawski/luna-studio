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
  --------
  describe "basic inputs" $ do
    it "empty file"                           $ parse [""]                 `shouldBe` []

  --------
  describe "entities" $ do
    describe "basic" $ do
      it "ident"                              $ parse ["ala"]              `shouldBe` [Identifier "ala"]
    
    describe "tuples" $ do
      it "empty"                              $ parse ["()"]               `shouldBe` [Tuple {items = []}]
      it "single"                             $ parse ["(a,)"]             `shouldBe` [Tuple {items = [Identifier "a"]}]
      it "double"                             $ parse ["(a,b,)"]           `shouldBe` [Tuple {items = [Identifier "a",Identifier "b"]}]

  --------
  describe "expressions" $ do
    describe "basic" $ do
      it "assignment"                         $ parse ["a=b"]              `shouldBe` [Assignment {src = Identifier "a", dst = Identifier "b"}]
      it "function call"                      $ parse ["a b c"]            `shouldBe` [Call {src = Identifier "a", args = [Identifier "b",Identifier "c"]}]
      it "accessors"                          $ parse ["a.b.c"]            `shouldBe` [Accessor {src = Accessor {src = Identifier "a", dst = Identifier "b"}, dst = Identifier "c"}]

    describe "functions" $ do
      it "empty"                              $ parse ["def f()"]          `shouldBe` [Function {name = "f", signature = [], body = []}]
      it "empty with signature"               $ parse ["def f(x,y,z)"]     `shouldBe` [Function {name = "f", signature = [Identifier "x",Identifier "y",Identifier "z"], body = []}]
      it "simple"                             $ parse ["def f(x):x"]       `shouldBe` [Function {name = "f", signature = [Identifier "x"], body = [Identifier "x"]}]
      it "multiline"                          $ parse ["def f(x):"
                                                      ,"    x+1"
                                                      ]                    `shouldBe` [Function {name = "f", signature = [Identifier "x"], body = [Operator {name = "+", src = Identifier "x", dst = Constant (Integer "1")}]}]
    describe "classes" $ do
      it "empty"                              $ parse ["class C"]          `shouldBe` [Class {name = "C", params = [], body = []}]
      it "empty with type variables"          $ parse ["class C a b c"]    `shouldBe` [Class {name = "C", params = ["a","b","c"], body = []}]
      it "simple"                             $ parse ["class C:"
                                                      ,"    i::Int"]       `shouldBe` [Class {name = "C", params = [], body = [Typed "Int" (Identifier "i")]}]
      it "double function"                    $ parse ["class A:"
                                                      ,"    def f(x):"
                                                      ,"        x"
                                                      ,"    def g(y):"
                                                      ,"        y"]        `shouldBe` [Class {name = "A", params = [], body = [Function {name = "f", signature = [Identifier "x"], body = [Identifier "x"]},Function {name = "g", signature = [Identifier "y"], body = [Identifier "y"]}]}]

    describe "lambdas" $ do
      it "empty"                              $ parse ["():()"]            `shouldBe` [Lambda {signature = [], body = [Tuple {items = []}]}]
      it "simple"                             $ parse ["x:x"]              `shouldBe` [Lambda {signature = [Identifier "x"], body = [Identifier "x"]}]
      it "nested"                             $ parse ["x: y: x+y"]        `shouldBe` [Lambda {signature = [Identifier "x"], body = [Lambda {signature = [Identifier "y"], body = [Operator {name = "+", src = Identifier "x", dst = Identifier "y"}]}]}]
      it "multiline"                          $ parse ["x:"
                                                      ,"    x+1"
                                                      ]                    `shouldBe` [Lambda {signature = [Identifier "x"], body = [Operator {name = "+", src = Identifier "x", dst = Constant (Integer "1")}]}]

  describe "other" $ do
    describe "lexing" $ do
      it "whitespaces test"                   $ parse ["class A:\n"
                                                      ,"    def f(x):\n"
                                                      ,"        x\n\n"
                                                      ,"    def g(y):\n"
                                                      ,"        y"]        `shouldBe` [Class {name = "A", params = [], body = [Function {name = "f", signature = [Identifier "x"], body = [Identifier "x"]},Function {name = "g", signature = [Identifier "y"], body = [Identifier "y"]}]}]

  describe "examples" $ do
    it "exception catch"                      $ parse [""
                                                      ,"a.catch IOError e:"
                                                      ,"    print e.message"
                                                      ]                    `shouldBe` [Call {src = Accessor {src = Identifier "a", dst = Identifier "catch"}, args = [Identifier "IOError",Lambda {signature = [Identifier "e"], body = [Call {src = Identifier "print", args = [Accessor {src = Identifier "e", dst = Identifier "message"}]}]}]}]

