---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE QuasiQuotes   #-}
{-# LANGUAGE TupleSections #-}

module Graph.GraphSpec where

import Test.Hspec
import Text.RawString.QQ

import           Flowbox.Control.Error
import qualified Flowbox.Luna.Data.AST.Crumb.Crumb                     as Crumb
import           Flowbox.Luna.Data.AST.Expr                            (Expr)
import qualified Flowbox.Luna.Data.AST.Zipper.Focus                    as Focus
import qualified Flowbox.Luna.Data.AST.Zipper.Zipper                   as Zipper
import           Flowbox.Luna.Data.Graph.Graph                         (Graph)
import           Flowbox.Luna.Data.Pass.AliasInfo                      (AliasInfo)
import           Flowbox.Luna.Data.Pass.Source                         (Source (Source))
import           Flowbox.Luna.Data.PropertyMap                         (PropertyMap)
import qualified Flowbox.Luna.Passes.Analysis.Alias.Alias              as Alias
import           Flowbox.Luna.Passes.Transform.AST.IDFixer.IDFixer     (clearIDs)
import qualified Flowbox.Luna.Passes.Transform.AST.TxtParser.TxtParser as TxtParser
import qualified Flowbox.Luna.Passes.Transform.Graph.Builder.Builder   as GraphBuilder
import qualified Flowbox.Luna.Passes.Transform.Graph.Parser.Parser     as GraphParser
import           Flowbox.Prelude



getAST :: String -> IO (Expr, AliasInfo)
getAST code = eitherStringToM' $ runEitherT $ do
    (ast, _, _) <- EitherT $ TxtParser.run $ Source ["Main"] code
    let bc = [Crumb.Module "Main", Crumb.Function "main" []]
    focus <- hoistEither $ Zipper.getFocus <$> Zipper.focusBreadcrumbs' bc ast
    aa    <- EitherT $ Alias.run ast
    expr  <- Focus.getFunction focus <??> "Target is not a function"
    return (expr, aa)


getGraph :: AliasInfo -> PropertyMap -> Expr -> IO (Graph, PropertyMap)
getGraph = eitherStringToM' .:. GraphBuilder.run


getExpr :: Graph -> PropertyMap -> Expr -> IO Expr
getExpr = eitherStringToM' .:. GraphParser.run


backAndForth :: String -> IO ()
backAndForth code = do
    (expr, aa)    <- getAST code
    (graph, pm)   <- getGraph aa def expr
    printLn
    print graph
    print pm
    printLn
    expr2         <- getExpr graph pm expr
    (graph2, pm2) <- getGraph aa def expr
    (clearIDs 0 expr2) `shouldBe` (clearIDs 0 expr)
    graph2 `shouldBe` graph
    pm2    `shouldBe` pm


named :: a -> b -> (a, b)
named = (,)


sampleCodes :: [(String,String)]
sampleCodes = [named "test1" [r|
def main:
    1
|], named "test2" [r|
def main:
    1 + 2
|], named "test3" [r|
def main:
    x = 0
|], named "test4" [r|
def main:
    1 + 2
    foo
|], named "test5" [r|
def main:
    foo
    2
|], named "test6" [r|
def main:
    foo.bar.baz
    2
|], named "test7" [r|
def main arg:
    arg.bar.baz
    2
|], named "test8" [r|
def main:
    x = foo.bar.baz
    2
|], named "test9" [r|
def main arg:
    foo.bar arg
    2
|], named "test10" [r|
def main arg:
    foo.bar arg 2
|], named "test11" [r|
def main arg:
    x = foo.bar(arg).baz arg 2
|], named "test12" [r|
def main arg:
    x = 4
    x.zooo 43
|], named "test13" [r|
def main arg:
    x.zooo 43
    -2
|]]



main :: IO ()
main = hspec spec


spec :: Spec
spec = do
    describe "graph conversion" $ do
        mapM_ (\(name, code) -> it ("returns the same when converting back and forth - " ++ name) $
                backAndForth code) sampleCodes
        --it "returns the same when converting back and forth" $ do
        --    mapM_ backAndForth sampleCodes


