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
import           Luna.Graph.Node.OutputName              (fixEmpty')
import qualified Luna.Graph.Port                         as Port
import           Luna.Pass.Transform.AST.IDFixer.IDFixer (clearIDs)
import qualified Test.Luna.AST.Common                    as Common
import           Test.Luna.Pass.Transform.Graph.Common   (named)
import qualified Test.Luna.Pass.Transform.Graph.Common   as Common
import           Test.Luna.SampleCodes                   (sampleCodes)
import qualified Test.Luna.SampleCodes                   as SampleCodes



backAndForth :: Breadcrumbs -> String -> IO ()
backAndForth bc code = do
    ast         <- Common.getAST code
    (graph, pm) <- Common.getGraph bc def ast
    --printLn
    --print ast
    --printLn
    --print graph
    --print pm
    --printLn
    (ast2  , pm2) <- Common.getExpr bc graph pm ast
    --print ast2
    --print pm
    (graph3, pm3) <- Common.getGraph bc pm2 ast2

    expr  <- Common.getMain (clearIDs 0 ast)
    expr2 <- Common.getMain (clearIDs 0 ast2)

    expr2  `shouldBe` expr
    graph3 `shouldBe` graph
    pm3    `shouldBe` pm2


backAndForth2 :: Breadcrumbs -> Graph -> IO ()
backAndForth2 bc graph = backAndForth2' bc graph graph


backAndForth2' :: Breadcrumbs -> Graph -> Graph -> IO ()
backAndForth2' bc providedGraph expectedGraph = do
    emptyAst  <- Common.getAST SampleCodes.emptyMain
    (ast, pm) <- Common.getExpr bc providedGraph def emptyAst
    --printLn
    --print ast
    --print pm
    (resultGraph, _pm2) <- Common.getGraph bc pm ast
    resultGraph `shouldBe` expectedGraph


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
    , named "simple graph 3"
    $ Graph.addMonadicEdges $ Graph.mkGraph
        [(-2, Node.Inputs         (0, 0))
        , fixEmpty' (100, Node.Expr "foo" "" (0, 1))
        , fixEmpty' (200, Node.Expr "bar" "" (0, 2))
        ,(-3, Node.Outputs        (0, 3))
        ]
        [(100, 200, Edge.Data Port.All $ Port.Num 5)]
    , named "simple graph 4"
    $ Graph.addMonadicEdges $ Graph.mkGraph
        [(-2, Node.Inputs         (0, 0))
        , fixEmpty' (100, Node.Expr "foo" "" (0, 1))
        , fixEmpty' (200, Node.Expr "bar" "" (0, 2))
        ,(-3, Node.Outputs        (0, 3))
        ]
        [(100, 200, Edge.Data Port.All $ Port.Num 5)
        ,(100, 200, Edge.Data Port.All $ Port.Num 3)
        ]
    , named "simple graph 4"
    $ Graph.addMonadicEdges $ Graph.mkGraph
        [(-2, Node.Inputs         (0, 0))
        , fixEmpty' (100, Node.Expr "foo" "" (0, 1))
        ,(-3, Node.Outputs        (0, 3))
        ]
        [(100, -3, Edge.Data Port.All $ Port.Num 2)
        ,(100, -3, Edge.Data Port.All $ Port.Num 3)
        ]
    , named "simple graph 5"
    $ Graph.addMonadicEdges $ Graph.mkGraph
        [(-2, Node.Inputs         (0, 0))
        , fixEmpty' (100, Node.Expr "main" "" (0, 1))
        ,(-3, Node.Outputs        (0, 3))
        ]
        [(100, -3, Edge.Data Port.All Port.All)]
    , named "simple graph 6"
    $ Graph.addMonadicEdges $ Graph.mkGraph
        [(-2, Node.Inputs         (0, 0))
        , fixEmpty' (100, Node.Expr "main" "" (0, 1))
        ,(-3, Node.Outputs        (0, 3))
        ]
        [(-2 ,100, Edge.Data (Port.Num 0) $ Port.Num 0)
        ,(100, -3, Edge.Data  Port.All      Port.All  )
        ]
    , named "inverse order graph"
    $ Graph.addMonadicEdges $ Graph.mkGraph
        [(-2, Node.Inputs         (0, 0))
        , fixEmpty' (100, Node.Expr "foo" "" (0, 1))
        , fixEmpty' (200, Node.Expr "bar" "" (0, 2))
        ,(-3, Node.Outputs        (0, 3))
        ]
        [(200, 100, Edge.Data Port.All $ Port.Num 5)]
    , named "graph with folded nodes"
    $ Graph.addMonadicEdges $ Graph.mkGraph
        [(-2, Node.Inputs         (0, 0))
        , fixEmpty' (100, Node.Expr "1 + 2" "" (0, 1))
        ,(-3, Node.Outputs        (0, 2))
        ]
        []
    ]


buggyGraphs :: [(String, Graph, Graph)]
buggyGraphs =
    [(  "empty"
     ,  Graph.addMonadicEdges $ Graph.mkGraph
        []
        []
     ,  Graph.addMonadicEdges $ Graph.mkGraph
        [(-2,Node.Inputs  (0 ,0))
        ,(-3,Node.Outputs (10,0))
        ]
        []
    ),( "buggy graph 1"
     ,  Graph.mkGraph
        [fixEmpty' (100, Node.Expr "main" "" (0, 1))]
        []
     ,  Graph.addMonadicEdges $ Graph.mkGraph
        [(-2,Node.Inputs           (0 , 0))
        ,(-3,Node.Outputs          (10, 1))
        ,fixEmpty' (100, Node.Expr "main" "" (0 , 1))
        ]
        [(100, -3, Edge.Data Port.All Port.All)]
    ),( "graph with [0] port descriptor on output"
     ,  Graph.addMonadicEdges $ Graph.mkGraph
        [(-2,Node.Inputs  (0 ,0))
        ,(-3,Node.Outputs (10,0))
        ,fixEmpty' (100, Node.Expr "foo" "" (0 , 1))
        ]
        [(100, -3, Edge.Data Port.All $ Port.Num 0)]
     ,  Graph.addMonadicEdges $ Graph.mkGraph
        [(-2,Node.Inputs  (0 ,0))
        ,(-3,Node.Outputs (10,0))
        ,fixEmpty' (100, Node.Expr "foo" "" (0 , 1))
        ]
        [(100, -3, Edge.Data Port.All Port.All)]
    ),( "graph with [0] and [1] port descriptors on output"
     ,  Graph.addMonadicEdges $ Graph.mkGraph
        [(-2,Node.Inputs  (0 ,0))
        ,(-3,Node.Outputs (10,0))
        ,fixEmpty' (100, Node.Expr "foo" "" (0 , 1))
        ]
        [(100, -3, Edge.Data Port.All $ Port.Num 0)
        ,(100, -3, Edge.Data Port.All $ Port.Num 1)
        ]
     ,  Graph.addMonadicEdges $ Graph.mkGraph
        [(-2,Node.Inputs  (0 ,0))
        ,(-3,Node.Outputs (10,0))
        ,fixEmpty' (100, Node.Expr "foo" "" (0 , 1))
        ]
        [(100, -3, Edge.Data Port.All $ Port.Num 0)
        ,(100, -3, Edge.Data Port.All $ Port.Num 1)
        ]
    ),( "graph with [] and [1] port descriptors on output - 1"
     ,  Graph.addMonadicEdges $ Graph.mkGraph
        [(-2,Node.Inputs  (0 ,0))
        ,(-3,Node.Outputs (10,0))
        ,fixEmpty' (100, Node.Expr "foo" "" (0 , 1))
        ]
        [(100, -3, Edge.Data Port.All $ Port.All)
        ,(100, -3, Edge.Data Port.All $ Port.Num 1)
        ]
     ,  Graph.addMonadicEdges $ Graph.mkGraph
        [(-2,Node.Inputs  (0 ,0))
        ,(-3,Node.Outputs (10,0))
        ,fixEmpty' (100, Node.Expr "foo" "" (0 , 1))
        ]
        [(100, -3, Edge.Data Port.All $ Port.Num 0)
        ,(100, -3, Edge.Data Port.All $ Port.Num 1)
        ]
    ),( "graph with [] and [1] port descriptors on output - 2"
     ,  Graph.addMonadicEdges $ Graph.mkGraph
        [(-2,Node.Inputs  (0 ,0))
        ,(-3,Node.Outputs (10,0))
        ,fixEmpty' (100, Node.Expr "foo" "" (0 , 1))
        ,fixEmpty' (200, Node.Expr "bar" "" (0 , 2))
        ]
        [(100, -3, Edge.Data Port.All $ Port.All)
        ,(200, -3, Edge.Data Port.All $ Port.Num 1)
        ]
     ,  Graph.addMonadicEdges $ Graph.mkGraph
        [(-2,Node.Inputs  (0 ,0))
        ,(-3,Node.Outputs (10,0))
        ,fixEmpty' (100, Node.Expr "foo" "" (0 , 1))
        ,fixEmpty' (200, Node.Expr "bar" "" (0 , 2))
        ]
        [(100, -3, Edge.Data Port.All $ Port.Num 0)
        ,(200, -3, Edge.Data Port.All $ Port.Num 1)
        ]
    ),( "graph with [], [1] and [2] port descriptors on output - 1"
     ,  Graph.addMonadicEdges $ Graph.mkGraph
        [(-2,Node.Inputs  (0 ,0))
        ,(-3,Node.Outputs (10,0))
        ,fixEmpty' (100, Node.Expr "foo" "" (0 , 1))
        ]
        [(100, -3, Edge.Data Port.All $ Port.All)
        ,(100, -3, Edge.Data Port.All $ Port.Num 1)
        ,(100, -3, Edge.Data Port.All $ Port.Num 2)
        ]
     ,  Graph.addMonadicEdges $ Graph.mkGraph
        [(-2,Node.Inputs  (0 ,0))
        ,(-3,Node.Outputs (10,0))
        ,fixEmpty' (100, Node.Expr "foo" "" (0 , 1))
        ]
        [(100, -3, Edge.Data Port.All $ Port.Num 0)
        ,(100, -3, Edge.Data Port.All $ Port.Num 1)
        ,(100, -3, Edge.Data Port.All $ Port.Num 2)
        ]
    ),( "graph with [], [1] and [2] port descriptors on output - 2"
     ,  Graph.addMonadicEdges $ Graph.mkGraph
        [(-2,Node.Inputs  (0 ,0))
        ,(-3,Node.Outputs (10,0))
        ,fixEmpty' (100, Node.Expr "foo" "" (0 , 1))
        ,fixEmpty' (200, Node.Expr "bar" "" (0 , 2))
        ]
        [(100, -3, Edge.Data Port.All $ Port.All)
        ,(200, -3, Edge.Data Port.All $ Port.Num 1)
        ,(100, -3, Edge.Data Port.All $ Port.Num 2)
        ]
     ,  Graph.addMonadicEdges $ Graph.mkGraph
        [(-2,Node.Inputs  (0 ,0))
        ,(-3,Node.Outputs (10,0))
        ,fixEmpty' (100, Node.Expr "foo" "" (0 , 1))
        ,fixEmpty' (200, Node.Expr "bar" "" (0 , 2))
        ]
        [(100, -3, Edge.Data Port.All $ Port.Num 0)
        ,(200, -3, Edge.Data Port.All $ Port.Num 1)
        ,(100, -3, Edge.Data Port.All $ Port.Num 2)
        ]
    )]

main :: IO ()
main = hspec spec


spec :: Spec
spec = do
    describe "ast <-> graph conversion" $ do
        mapM_ (\(name, code) -> it ("returns the same when converting back and forth - " ++ name) $
                backAndForth Common.mainBC code) sampleCodes
        mapM_ (\(name, bc, code) -> it ("returns the same when converting back and forth - " ++ name) $
                backAndForth bc code) SampleCodes.sampleLambdas

    describe "graph <-> ast conversion" $ do
        mapM_ (\(name, graph) -> it ("returns the same when converting back and forth - " ++ name) $
                backAndForth2 Common.mainBC graph) sampleGraphs
        mapM_ (\(name, providedGraph, expectedGraph) -> it ("fixes buggy graphs - " ++ name) $
                backAndForth2' Common.mainBC providedGraph expectedGraph) buggyGraphs


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
                edges  = [(1, 2, Edge.Data Port.All $ Port.Num 0)
                         ,(5, 3, Edge.Data Port.All $ Port.Num 0)]
                graph  = Graph.mkGraph testOrder edges
                sorted = Graph.sort graph
            map fst sorted `shouldBe` map fst properOrder
            sorted         `shouldBe` properOrder
