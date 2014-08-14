---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------

module Graph.GraphViewSpec where

import Test.Hspec

import           Flowbox.Control.Error
import qualified Flowbox.Luna.Data.Graph.Node          as Node
import           Flowbox.Luna.Data.GraphView.EdgeView  (EdgeView (EdgeView))
import           Flowbox.Luna.Data.GraphView.GraphView (GraphView)
import qualified Flowbox.Luna.Data.GraphView.GraphView as GraphView
import           Flowbox.Prelude
import           Graph.Common                          (named)
import qualified Graph.Common                          as Common
import           Graph.SampleCodes                     (sampleCodes)



backAndForth :: String -> IO ()
backAndForth code = do
    expr        <- Common.getAST code
    (graph    , pm) <- Common.getGraph def expr
    let (graphview, pm2) = GraphView.fromGraph graph pm
    (graph3   , pm3) <- eitherStringToM $ GraphView.toGraph graphview pm2

    graph `shouldBe` graph3
    pm    `shouldBe` pm3


backAndForth2 :: GraphView -> IO ()
backAndForth2 graphview = do
    let emptyPM    = def
    --printLn
    --print graphview
    (graph, pm) <- eitherStringToM $ GraphView.toGraph graphview emptyPM
    --printLn
    --print graph
    --printLn
    let (graphview2, pm2) = GraphView.fromGraph graph pm

    graphview2 `shouldBe` graphview
    pm2        `shouldBe` emptyPM


main :: IO ()
main = hspec spec


sampleGraphs :: [(String, GraphView)]
sampleGraphs =
    [ named "simple graphview"
    $ GraphView.mkGraph
        [(-1, Node.Inputs  (0, 0))
        ,(-2, Node.Outputs (0, 0))
        ,(0 , Node.Expr "foo" "" (0, 0))
        ,(1 , Node.Expr "bar" "" (1, 1))
        ,(2 , Node.Expr "baz" "" (0, 0))
        ]
        [(0 , 1, EdgeView [0] [1])
        ,(0 , 1, EdgeView []  [2])
        ,(1 , 2, EdgeView [1] [0])
        ,(1 , 2, EdgeView []  [3])
        ]
    , named "graphview with in-ports"
    $ GraphView.mkGraph
        [(-1, Node.Inputs  (0, 0))
        ,(-2, Node.Outputs (0, 0))
        ,(0 , Node.Expr "foo" "" (0, 0))
        ,(1 , Node.Expr "bar" "" (1, 1))
        ,(2 , Node.Expr "baz" "" (0, 0))
        ]
        [(0, 1, EdgeView [0, 2] [1])
        ,(0, 1, EdgeView [] [2])
        ,(1, 2, EdgeView [] [0])
        ,(1, 2, EdgeView [1, 3, 4] [3])
        ]
    , named "graphview with out-ports"
    $ GraphView.mkGraph
        [(-1, Node.Inputs  (0, 0))
        ,(-2, Node.Outputs (0, 0))
        ,(0 , Node.Expr "foo" "" (0, 0))
        ,(1 , Node.Expr "bar" "" (1, 1))
        ,(2 , Node.Expr "baz" "" (0, 0))
        ]
        [(0, 1, EdgeView [0] [1])
        ,(0, 1, EdgeView []  [2])
        ,(1, 2, EdgeView [1] [0, 1, 2])
        ,(1, 2, EdgeView []  [3, 5, 7, 9])
        ]
    , named "graphview with in-ports and out-ports"
    $ GraphView.mkGraph
        [(-1, Node.Inputs  (0, 0))
        ,(-2, Node.Outputs (0, 0))
        ,(0 , Node.Expr "foo" "" (0, 0))
        ,(1 , Node.Expr "bar" "" (1, 1))
        ,(2 , Node.Expr "baz" "" (0, 0))
        ]
        [(0, 1, EdgeView [0, 2]    [1])
        ,(0, 1, EdgeView [1, 2, 3] [2])
        ,(1, 2, EdgeView []        [0, 1, 2])
        ,(1, 2, EdgeView [1, 3, 4] [3, 5, 7, 9])
        ]
    ]


spec :: Spec
spec = do
    describe "code -> graph <-> graphview conversion" $ do
        mapM_ (\(name, code) -> it ("returns the same when converting back and forth - " ++ name) $
                backAndForth code) sampleCodes


    describe "graphview <-> graph conversion" $ do
        mapM_ (\(name, gv) -> it ("returns the same when converting back and forth - " ++ name) $
                backAndForth2 gv) sampleGraphs
