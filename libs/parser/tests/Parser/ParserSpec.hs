---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------

module Parser.ParserSpec where

import           Test.Hspec                   
import           Data.Either.Utils            (forceEither)
import qualified Flowbox.Luna.Parser.Parser as Parser
import Flowbox.Luna.Parser.AST.AST -- for tests
import Flowbox.Luna.Parser.AST.Constant -- for tests
--import Flowbox.Luna.Parser.AST.Type -- for tests


main :: IO ()
main = hspec spec

parse :: [String] -> String
parse s = show $ body $ forceEither $ Parser.parse $ unlines s

spec :: Spec
spec = do
  -------- BASIC --------
  describe "basic inputs" $ do
    it "empty file"                           $ parse [""]                 `shouldBe` "[]"
    it "single comment"                       $ parse ["#comment"]         `shouldBe` "[]"
    it "one line comment"                     $ parse ["a=1 #comment"]     `shouldBe` "[Assignment {src = Pattern (Identifier \"a\"), dst = Constant (Integer \"1\")}]"
    it "multiline comment"                    $ parse ["a#[comment\n#]\nb"]`shouldBe` "[Identifier \"a\",Identifier \"b\"]"
    it "nested comments"                      $ parse ["a #["
                                                      ,"commented line"
                                                      ,"#[ nested start"
                                                      ,"#] nested end"
                                                      ,"#]"
                                                      ,"b"]                `shouldBe` "[Identifier \"a\",Identifier \"b\"]"

  -------- ENTITIES --------
  describe "entities" $ do
    describe "basic" $ do
      it "ident"                              $ parse ["ala"]              `shouldBe` "[Identifier \"ala\"]"
      it "character literal"                  $ parse ["'a'"]              `shouldBe` "[Constant (Char 'a')]"
      it "empty string literal"               $ parse ["\"\""]             `shouldBe` "[Constant (String \"\")]"
      it "string literal"                     $ parse ["\"test\""]         `shouldBe` "[Constant (String \"test\")]"
    
    describe "tuples" $ do
      it "empty"                              $ parse ["()"]               `shouldBe` "[Tuple {items = []}]"
      it "single"                             $ parse ["(a,)"]             `shouldBe` "[Tuple {items = [Identifier \"a\"]}]"
      it "double"                             $ parse ["(a,b,)"]           `shouldBe` "[Tuple {items = [Identifier \"a\",Identifier \"b\"]}]"

  -------- EXPRESSIONS --------
  describe "expressions" $ do
    describe "imports" $ do
      it "single import"                      $ parse ["import Std.Math.Vector"]           `shouldBe` "[Import {paths = [Path {segments = [\"Std\",\"Math\",\"Vector\"]}]}]"
      it "single named import"                $ parse ["import Std.Math.Vector as Vector"] `shouldBe` "[Import {paths = [Named {name = \"Vector\", item = Path {segments = [\"Std\",\"Math\",\"Vector\"]}}]}]"
      it "multi import"                       $ parse ["import Std.Math.Vector as Vector"
                                                      ,"       Std.Math.Scalar as Scalar"
                                                      ]                                    `shouldBe` "[Import {paths = [Named {name = \"Vector\", item = Path {segments = [\"Std\",\"Math\",\"Vector\"]}},Named {name = \"Scalar\", item = Path {segments = [\"Std\",\"Math\",\"Scalar\"]}}]}]"
      it "qualified import"                   $ parse ["from Std.Math import Vector as V"
                                                      ,"                     Scalar as S"
                                                      ]                                    `shouldBe` "[ImportQualified {path = Path {segments = [\"Std\",\"Math\"]}, imports = Import {paths = [Named {name = \"V\", item = Path {segments = [\"Vector\"]}},Named {name = \"S\", item = Path {segments = [\"Scalar\"]}}]}}]"

    describe "basic" $ do
      it "assignment"                         $ parse ["a=b"]              `shouldBe` "[Assignment {src = Pattern (Identifier \"a\"), dst = Identifier \"b\"}]"
      it "function call"                      $ parse ["a b c"]            `shouldBe` "[Call {src = Identifier \"a\", args = [Identifier \"b\",Identifier \"c\"]}]"
      it "accessors"                          $ parse ["a.b.c"]            `shouldBe` "[Accessor {src = Accessor {src = Identifier \"a\", dst = Identifier \"b\"}, dst = Identifier \"c\"}]"

    describe "functions" $ do
      it "empty"                              $ parse ["def f()"]          `shouldBe` "[Function {name = \"f\", signature = Lambda {inputs = Tuple {items = []}, outputs = Unknown}, body = []}]"
      it "empty with signature"               $ parse ["def f(x,y,z)"]     `shouldBe` "[Function {name = \"f\", signature = Lambda {inputs = Tuple {items = [Type {name = \"x\"},Type {name = \"y\"},Type {name = \"z\"}]}, outputs = Unknown}, body = []}]"
      --it "simple"                             $ parse ["def f(x):x"]       `shouldBe` "[]"
      it "multiline"                          $ parse ["def f(x):"
                                                      ,"    x+1"
                                                      ]                    `shouldBe` "[Function {name = \"f\", signature = Lambda {inputs = Tuple {items = [Type {name = \"x\"}]}, outputs = Unknown}, body = [Operator {name = \"+\", src = Identifier \"x\", dst = Constant (Integer \"1\")}]}]"
    describe "classes" $ do
      it "empty"                              $ parse ["class C"]          `shouldBe` "[Class {cls = Class {name = \"C\", params = []}, fields = [], methods = []}]"
      it "empty with type variables"          $ parse ["class C a b c"]    `shouldBe` "[Class {cls = Class {name = \"C\", params = [\"a\",\"b\",\"c\"]}, fields = [], methods = []}]"
      it "simple"                             $ parse ["class C:"
                                                      ,"    i::Int"]       `shouldBe` "[Class {cls = Class {name = \"C\", params = []}, fields = [Field {name = \"i\", cls = Type {name = \"Int\"}}], methods = []}]"
      it "double function"                    $ parse ["class A:"
                                                      ,"    def f(x):"
                                                      ,"        x"
                                                      ,"    def g(y):"
                                                      ,"        y"]        `shouldBe` "[Class {cls = Class {name = \"A\", params = []}, fields = [], methods = [Function {name = \"f\", signature = Lambda {inputs = Tuple {items = [Type {name = \"x\"}]}, outputs = Unknown}, body = [Identifier \"x\"]},Function {name = \"g\", signature = Lambda {inputs = Tuple {items = [Type {name = \"y\"}]}, outputs = Unknown}, body = [Identifier \"y\"]}]}]"

    --describe "lambdas" $ do
    --  it "empty"                              $ parse ["():()"]            `shouldBe` [Lambda {signature = [], body = [Tuple {items = []}]}]
    --  it "simple"                             $ parse ["x:x"]              `shouldBe` [Lambda {signature = [Identifier "x"], body = [Identifier "x"]}]
    --  it "nested"                             $ parse ["x: y: x+y"]        `shouldBe` [Lambda {signature = [Identifier "x"], body = [Lambda {signature = [Identifier "y"], body = [Operator {name = "+", src = Identifier "x", dst = Identifier "y"}]}]}]
    --  it "multiline"                          $ parse ["x:"
    --                                                  ,"    x+1"
    --                                                  ]                    `shouldBe` [Lambda {signature = [Identifier "x"], body = [Operator {name = "+", src = Identifier "x", dst = Constant (Integer "1")}]}]

  -------- OTHER --------
  describe "other" $ do
    describe "lexing" $ do
      it "whitespaces test"                   $ parse ["class A:\n"
                                                      ,"    def f(x):\n"
                                                      ,"        x\n\n"
                                                      ,"    def g(y):\n"
                                                      ,"        y"]        `shouldBe` "[Class {cls = Class {name = \"A\", params = []}, fields = [], methods = [Function {name = \"f\", signature = Lambda {inputs = Tuple {items = [Type {name = \"x\"}]}, outputs = Unknown}, body = [Identifier \"x\"]},Function {name = \"g\", signature = Lambda {inputs = Tuple {items = [Type {name = \"y\"}]}, outputs = Unknown}, body = [Identifier \"y\"]}]}]"

  ---------- EXAMPLES --------
  --describe "examples" $ do
  --  it "exception catch"                      $ parse [""
  --                                                    ,"a.catch IOError e:"
  --                                                    ,"    print e.message"
  --                                                    ]                    `shouldBe` "[]"
