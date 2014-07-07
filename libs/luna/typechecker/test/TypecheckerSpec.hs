{-# LANGUAGE QuasiQuotes #-}

module TypecheckerSpec (main, spec) where

import Flowbox.Typechecker.Basic

--import qualified Data.Maybe as Maybe
import           Data.Maybe                                            (isJust,isNothing)
import           Text.RawString.QQ
import           Control.Monad.IO.Class                                (MonadIO)
import           Control.Monad.Trans                                   (lift)


import Test.Hspec

import qualified Flowbox.Luna.Data.AST.AST                             as AST
import qualified Flowbox.Luna.Data.AST.Common                          as Common
import qualified Flowbox.Luna.Data.AST.Expr                            as Expr
import qualified Flowbox.Luna.Data.AST.Lit                             as Lit
import qualified Flowbox.Luna.Data.AST.Module                          as Module
import qualified Flowbox.Luna.Data.AST.Pat                             as Pat
import qualified Flowbox.Luna.Data.AST.Type                            as Type
import           Flowbox.Control.Error                                 (eitherStringToM)
import           Flowbox.Luna.Data.Pass.Source                         (Source (Source))
import qualified Flowbox.Luna.Passes.Transform.AST.TxtParser.TxtParser as TxtParser
import qualified Flowbox.System.UniPath                                as UniPath




main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "black-box specification" $ do
    it "works for empty ASTs" $
      (typecheck $ Module.Module {}) `shouldSatisfy` isJust
    it "works for actually empty ASTs" $
      (typecheck $ Module.Module {
        Module._id = 111, Module._cls = Type.Unknown { Type._id = 123 }
      }) `shouldSatisfy` isJust
    it "works for ASTs with (incorrectly) doubling IDs for non-equal elements" $ pending
    it "works for ASTs with doubling IDs for equal elements"                   $ pending
  describe "parses trivial programs" $ do
    it "works for the example trivial_01" $ do
      result <- (return . typecheck) =<< textToAST luna_trivial_01
      result `shouldSatisfy` isJust
    it "works for the example trivial_02" $ do
      result <- (return . typecheck) =<< textToAST luna_trivial_02
      result `shouldSatisfy` isJust
    it "works for the example trivial_03 (error)" $ do
      result <- (return . typecheck) =<< textToAST luna_trivial_03
      result `shouldSatisfy` isNothing

textToAST :: (Monad m, MonadIO m, Functor m) => String -> m Module.Module
textToAST str = do
    let code = Source ["Main"] str
        path = UniPath.fromUnixString "."
    (source, _, _) <- eitherStringToM =<< TxtParser.run code
    return source

luna_trivial_01 :: String
luna_trivial_01 = [r|
def foo:
  2 + 2
|]

luna_trivial_02 :: String
luna_trivial_02 = [r|
def foo:
  2 + 2

def bar:
  foo + 5
|]

luna_trivial_03 :: String
luna_trivial_03 = [r|
def foo:
  2 + 2

def bar:
  foo + "foobar"
|]