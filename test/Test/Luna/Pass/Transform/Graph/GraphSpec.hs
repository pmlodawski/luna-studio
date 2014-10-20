---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------

module Test.Luna.Pass.Transform.Graph.GraphSpec where

import Test.Hspec

import           Flowbox.Prelude
import           Luna.AST.Control.Crumb                  (Breadcrumbs)
import qualified Luna.Graph.Edge                         as Edge
import           Luna.Graph.Graph                        (Graph)
import qualified Luna.Graph.Graph                        as Graph
import qualified Luna.Graph.Node                         as Node
import qualified Luna.Graph.Node.Expr                    as NodeExpr
import qualified Luna.Graph.Port                         as Port
import           Luna.Pass.Transform.AST.IDFixer.IDFixer (clearIDs)
import qualified Test.Luna.AST.Common                    as Common
import qualified Test.Luna.Pass.Transform.Graph.Common   as Common
import           Test.Luna.Sample.Code                   (sampleCodes)
import qualified Test.Luna.Sample.Code                   as SampleCode
import           Test.Luna.Sample.Graph                  (buggyGraphs, sampleGraphs)
import           Text.Show.Pretty



backAndForth :: Breadcrumbs -> String -> IO ()
backAndForth bc code = do
    ast         <- Common.getAST code
    --putStrLn "== getGraph"
    (graph, pm) <- Common.getGraph bc def ast
    --printLn
    --print ast
    --printLn
    --putStrLn $ ppShow graph
    --printLn
    --print pm
    --printLn
    --putStrLn "== getExpr"
    (ast2  , pm2) <- Common.getExpr bc graph pm ast
    --print ast2
    --printLn
    --print pm2
    --printLn
    --putStrLn "== getGraph"
    (graph3, pm3) <- Common.getGraph bc pm2 ast2
    --print pm3
    --printLn
    expr  <- Common.getMain (clearIDs 0 ast)
    expr2 <- Common.getMain (clearIDs 0 ast2)

    expr2  `shouldBe` expr
    graph3 `shouldBe` graph
    pm3    `shouldBe` pm2


backAndForth2 :: Breadcrumbs -> Graph -> IO ()
backAndForth2 bc graph = backAndForth2' bc graph graph


backAndForth2' :: Breadcrumbs -> Graph -> Graph -> IO ()
backAndForth2' bc providedGraph expectedGraph = do
    emptyAst  <- Common.getAST SampleCode.emptyMain
    (ast, pm) <- Common.getExpr bc providedGraph def emptyAst
    --printLn
    --print ast
    --printLn
    --print pm
    --printLn
    (resultGraph, _pm2) <- Common.getGraph bc pm ast
    resultGraph `shouldBe` expectedGraph


main :: IO ()
main = hspec spec


spec :: Spec
spec = do
    describe "ast <-> graph conversion" $ do
        mapM_ (\(name, code) -> it ("returns the same when converting back and forth - " ++ name) $
                backAndForth Common.mainBC code) sampleCodes
        mapM_ (\(name, bc, code) -> it ("returns the same when converting back and forth - " ++ name) $
                backAndForth bc code) SampleCode.sampleLambdas

    describe "graph <-> ast conversion" $ do
        mapM_ (\(name, graph) -> it ("returns the same when converting back and forth - " ++ name) $
                backAndForth2 Common.mainBC graph) sampleGraphs
        mapM_ (\(name, providedGraph, expectedGraph) -> it ("fixes buggy graphs - " ++ name) $
                backAndForth2' Common.mainBC providedGraph expectedGraph) buggyGraphs


    describe "graph sort alghorithm" $ do
        it "sorts graph correctly" $ do
            let n1 = (1, Node.Expr (NodeExpr.Expr "") "" (1, 0))
                n2 = (2, Node.Expr (NodeExpr.Expr "") "" (2, 0))
                n3 = (3, Node.Expr (NodeExpr.Expr "") "" (2, 0))
                n4 = (4, Node.Expr (NodeExpr.Expr "") "" (3, 0))
                n5 = (5, Node.Expr (NodeExpr.Expr "") "" (4, 0))
                n6 = (6, Node.Expr (NodeExpr.Expr "") "" (5, 0))
                properOrder = [n1, n2, n4, n5, n3, n6]
                testOrder   = [n2, n3, n5, n6, n4, n1]
                edges  = [(1, 2, Edge.Data Port.All $ Port.Num 0)
                         ,(5, 3, Edge.Data Port.All $ Port.Num 0)]
                graph  = Graph.mkGraph testOrder edges
                sorted = Graph.sort graph
            map fst sorted `shouldBe` map fst properOrder
            sorted         `shouldBe` properOrder
