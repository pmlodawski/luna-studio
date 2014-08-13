---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------

module Graph.GraphSpec where

import Test.Hspec

import qualified Flowbox.Luna.Data.Graph.Edge                        as Edge
import           Flowbox.Luna.Data.Graph.Graph                       (Graph)
import qualified Flowbox.Luna.Data.Graph.Graph                       as Graph
import qualified Flowbox.Luna.Data.Graph.Node                        as Node
import qualified Flowbox.Luna.Data.Graph.Port                        as Port
import           Flowbox.Luna.Passes.Transform.AST.IDFixer.IDFixer   (clearIDs)
import           Flowbox.Luna.Passes.Transform.Graph.Node.OutputName (fixEmpty')
import           Flowbox.Prelude
import           Graph.Common                                        (named)
import qualified Graph.Common                                        as Common
import           Graph.SampleCodes                                   (sampleCodes)
import qualified Graph.SampleCodes                                   as SampleCodes


backAndForth :: String -> IO ()
backAndForth code = do
    ast         <- Common.getAST code
    (graph, pm) <- Common.getGraph def ast
    --printLn
    --print ast
    --printLn
    --print graph
    --print pm
    --printLn
    (ast2  , pm2) <- Common.getExpr graph pm ast
    (graph3, pm3) <- Common.getGraph pm2 ast2

    expr  <- Common.getMain (clearIDs 0 ast)
    expr2 <- Common.getMain (clearIDs 0 ast2)

    expr2  `shouldBe` expr
    graph3 `shouldBe` graph
    pm3    `shouldBe` pm2


backAndForth2 :: Graph -> IO ()
backAndForth2 graph = do
    emptyAst <- Common.getAST SampleCodes.emptyMain
    --printLn
    --print emptyAst
    --printLn
    --print graph
    (ast, pm) <- Common.getExpr graph def emptyAst
    --printLn
    --print ast
    (graph2, pm2) <- Common.getGraph pm ast
    --printLn
    --print graph2
    --printLn
    --print pm
    --printLn
    graph2 `shouldBe` graph

    --ast2           <- Common.getExpr graph2 pm ast
    --(graph2, pm)    <- Common.getGraph aa pm ast2


    --pm2    `shouldBe` pm


sampleGraphs :: [(String, Graph)]
sampleGraphs =
    [ named "simple graph 1"
    $ Graph.addMonadicEdges $ Graph.mkGraph
        [(-2, Node.Inputs         (0, 0))
        , fixEmpty' (100, Node.Expr "main2" "" (0, 1))
        ,(-3, Node.Outputs        (0, 2))
        ]
        []
    , named "simple graph 2"
    $ Graph.addMonadicEdges $ Graph.mkGraph
        [(-2, Node.Inputs         (0, 0))
        , fixEmpty' (100, Node.Expr "main" "" (0, 1))
        ,(-3, Node.Outputs        (0, 2))
        ]
        []
    ]


main :: IO ()
main = hspec spec


spec :: Spec
spec = do
    describe "ast <-> graph conversion" $ do
        mapM_ (\(name, code) -> it ("returns the same when converting back and forth - " ++ name) $
                backAndForth code) sampleCodes

    describe "graph <-> ast conversion" $ do
        mapM_ (\(name, code) -> it ("returns the same when converting back and forth - " ++ name) $
                backAndForth2 code) sampleGraphs

    describe "graph sort alghorithm" $ do
        it "sorts graph correctly" $ do
            let n1 = (1, Node.Expr "" "" (1, 0))
                n2 = (2, Node.Expr "" "" (2, 0))
                n3 = (3, Node.Expr "" "" (2, 0))
                n4 = (4, Node.Expr "" "" (3, 0))
                n5 = (5, Node.Expr "" "" (4, 0))
                n6 = (6, Node.Expr "" "" (5, 0))
                properOrder = [n1, n2, n4, n5, n3, n6]
                testOrder   = [n2, n3, n5, n6, n4, n1]
                edges  = [(1, 2, Edge.Data Port.All 0)
                         ,(5, 3, Edge.Data Port.All 0)]
                graph  = Graph.mkGraph testOrder edges
                sorted = Graph.sort graph
            map fst sorted `shouldBe` map fst properOrder
            sorted         `shouldBe` properOrder
