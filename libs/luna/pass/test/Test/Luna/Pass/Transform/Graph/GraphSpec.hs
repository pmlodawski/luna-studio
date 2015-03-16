---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------

module Test.Luna.Pass.Transform.Graph.GraphSpec where

import Test.Hspec

import           Flowbox.Prelude
import           Luna.Syntax.Control.Crumb             (Breadcrumbs)
import qualified Luna.Syntax.Graph.Tag                 as Tag
import qualified Luna.Util.Label                       as Label
import qualified Test.Luna.Pass.Transform.Graph.Common as Common
import           Test.Luna.Sample.Code                 (sampleCodes)
import qualified Test.Luna.Sample.Code                 as SampleCode
import qualified Test.Luna.Syntax.Common               as Common
--import qualified Luna.Syntax.Graph.Edge                  as Edge
--import           Luna.Syntax.Graph.Graph                 (Graph)
--import qualified Luna.Syntax.Graph.Graph                 as Graph
--import qualified Luna.Syntax.Graph.Node                  as Node
--import           Luna.Syntax.Graph.Node.Expr             (NodeExpr)
--import qualified Luna.Syntax.Graph.Node.Expr             as NodeExpr
--import qualified Luna.Syntax.Graph.Node.StringExpr       as StringExpr
--import qualified Luna.Syntax.Graph.Port                  as Port
--import           Test.Luna.Sample.Graph                  (buggyGraphs, sampleGraphs)



--strExpr :: String -> NodeExpr
--strExpr = NodeExpr.StringExpr . StringExpr.Expr


backAndForth :: Breadcrumbs -> String -> IO ()
backAndForth bc code = do
    ast'        <- Common.getAST code
    let ast = Label.replace Tag.fromEnumerated ast'
    putStrLn "== getGraph"

    (ast, graph) <- Common.getGraph bc ast
    printLn
    prettyPrint ast
    printLn
    prettyPrint graph
    printLn
    putStrLn "== getExpr"
    ast2   <- Common.getExpr bc graph ast
    --print ast2
    --printLn
    --print pm2
    --printLn
    --putStrLn "== getGraph"
    (ast2, graph3) <- Common.getGraph bc ast2
    --print pm3
    --printLn
    expr  <- Common.getMain ast
    expr2 <- Common.getMain ast2

    expr2  `shouldBe` expr
    graph3 `shouldBe` graph


--backAndForth2 :: Breadcrumbs -> Graph -> IO ()
--backAndForth2 bc graph = backAndForth2' bc graph graph


--backAndForth2' :: Breadcrumbs -> Graph -> Graph -> IO ()
--backAndForth2' bc providedGraph expectedGraph = do
--    emptyAst  <- Common.getAST SampleCode.emptyMain
--    (ast, pm) <- Common.getExpr bc providedGraph def emptyAst
--    --printLn
--    --print ast
--    --printLn
--    --print pm
--    --printLn
--    (resultGraph, _pm2) <- Common.getGraph bc pm ast
--    resultGraph `shouldBe` expectedGraph


main :: IO ()
main = hspec spec


spec :: Spec
spec = do
    describe "ast <-> graph conversion" $ do
        it "" pending
        mapM_ (\(name, code) -> it ("returns the same when converting back and forth - " ++ name) $
                backAndForth Common.mainBC code) sampleCodes
        --mapM_ (\(name, bc, code) -> it ("returns the same when converting back and forth - " ++ name) $
        --        backAndForth bc code) SampleCode.sampleLambdas

    describe "graph <-> ast conversion" $ do
        it "" pending
        --mapM_ (\(name, graph) -> it ("returns the same when converting back and forth - " ++ name) $
        --        backAndForth2 Common.mainBC graph) sampleGraphs
        --mapM_ (\(name, providedGraph, expectedGraph) -> it ("fixes buggy graphs - " ++ name) $
        --        backAndForth2' Common.mainBC providedGraph expectedGraph) buggyGraphs


    describe "graph sort alghorithm" $ do
        it "" pending
        --it "sorts graph correctly" $ do
        --    let n1 = (1, Node.Expr (strExpr "") "" (1, 0))
        --        n2 = (2, Node.Expr (strExpr "") "" (2, 0))
        --        n3 = (3, Node.Expr (strExpr "") "" (2, 0))
        --        n4 = (4, Node.Expr (strExpr "") "" (3, 0))
        --        n5 = (5, Node.Expr (strExpr "") "" (4, 0))
        --        n6 = (6, Node.Expr (strExpr "") "" (5, 0))
        --        properOrder = [n1, n2, n4, n5, n3, n6]
        --        testOrder   = [n2, n3, n5, n6, n4, n1]
        --        edges  = [(1, 2, Edge.Data Port.All $ Port.Num 0)
        --                 ,(5, 3, Edge.Data Port.All $ Port.Num 0)]
        --        graph  = Graph.mkGraph testOrder edges
        --        sorted = Graph.sort graph
        --    map fst sorted `shouldBe` map fst properOrder
        --    sorted         `shouldBe` properOrder
