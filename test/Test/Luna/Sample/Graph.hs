---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------

module Test.Luna.Sample.Graph where

import           Flowbox.Prelude
import qualified Luna.Graph.Edge                       as Edge
import           Luna.Graph.Graph                      (Graph)
import qualified Luna.Graph.Graph                      as Graph
import qualified Luna.Graph.Node                       as Node
import           Luna.Graph.Node.OutputName            (fixEmpty')
import qualified Luna.Graph.Port                       as Port
import           Test.Luna.Pass.Transform.Graph.Common (named)



sampleGraphs :: [(String, Graph)]
sampleGraphs =
    [ named "simple graph 1"
    $ Graph.addMonadicEdges $ Graph.mkGraph
        [(-2, Node.Inputs         (0, 0))
        , fixEmpty' (100, Node.Expr "foo" "" (0, 1))
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
    , named "simple graph 5"
    $ Graph.addMonadicEdges $ Graph.mkGraph
        [(-2, Node.Inputs         (0, 0))
        , fixEmpty' (100, Node.Expr "foo" "" (0, 1))
        ,(-3, Node.Outputs        (0, 3))
        ]
        [(100, -3, Edge.Data Port.All $ Port.Num 2)
        ,(100, -3, Edge.Data Port.All $ Port.Num 3)
        ]
    , named "simple graph 6"
    $ Graph.addMonadicEdges $ Graph.mkGraph
        [(-2, Node.Inputs         (0, 0))
        , fixEmpty' (100, Node.Expr "main" "" (0, 1))
        ,(-3, Node.Outputs        (0, 3))
        ]
        [(100, -3, Edge.Data Port.All Port.All)]
    , named "simple graph 7"
    $ Graph.addMonadicEdges $ Graph.mkGraph
        [(-2, Node.Inputs         (0, 0))
        , fixEmpty' (100, Node.Expr "main" "" (0, 1))
        ,(-3, Node.Outputs        (0, 3))
        ]
        [(-2 ,100, Edge.Data (Port.Num 0) $ Port.Num 0)
        ,(100, -3, Edge.Data  Port.All      Port.All  )
        ]
    , named "simple graph 8"
    $ Graph.addMonadicEdges $ Graph.mkGraph
        [(-2, Node.Inputs         (0, 0))
        , fixEmpty' (100, Node.Expr "25" "" (0, 1))
        , fixEmpty' (200, Node.Expr "*" "" (0, 1))
        ,(-3, Node.Outputs        (0, 3))
        ]
        [(100,200, Edge.Data Port.All $ Port.Num 0)
        ,(100,200, Edge.Data Port.All $ Port.Num 1)
        ]
    , named "inverse order graph"
    $ Graph.addMonadicEdges $ Graph.mkGraph
        [(-2, Node.Inputs         (0, 0))
        , fixEmpty' (100, Node.Expr "foo" "" (0, 1))
        , fixEmpty' (200, Node.Expr "bar" "" (0, 2))
        ,(-3, Node.Outputs        (0, 3))
        ]
        [(200, 100, Edge.Data Port.All $ Port.Num 5)]
    -- , named "graph with folded nodes 1"
    -- $ Graph.addMonadicEdges $ Graph.mkGraph
    --    [(-2, Node.Inputs         (0, 0))
    --    , fixEmpty' (100, Node.Expr "1 * 2 * 3" "" (0, 1))
    --    ,(-3, Node.Outputs        (0, 2))
    --    ]
    --    []
    , named "graph with folded nodes 2"
    $ Graph.addMonadicEdges $ Graph.mkGraph
        [(-2, Node.Inputs         (0, 0))
        , fixEmpty' (100, Node.Expr "1.+ 2" "" (0, 1))
        ,(-3, Node.Outputs        (0, 2))
        ]
        []
    , named "graph with folded nodes 3"
    $ Graph.addMonadicEdges $ Graph.mkGraph
        [(-2, Node.Inputs         (0, 0))
        , fixEmpty' (100, Node.Expr "1.+ 2.* 2" "" (0, 1))
        ,(-3, Node.Outputs        (0, 2))
        ]
        []
    , named "graph with folded nodes 4"
    $ Graph.addMonadicEdges $ Graph.mkGraph
        [(-2, Node.Inputs         (0, 0))
        , fixEmpty' (100, Node.Expr "(1.+ 2).* 2" "" (0, 1))
        ,(-3, Node.Outputs        (0, 2))
        ]
        []
    , named "graph with folded nodes 5"
    $ Graph.addMonadicEdges $ Graph.mkGraph
        [(-2, Node.Inputs         (0, 0))
        , fixEmpty' (100, Node.Expr "a.foo bar 1 \"asda\"" "" (0, 1))
        ,(-3, Node.Outputs        (0, 2))
        ]
        []
     , named "graph with folded nodes 6"
     $ Graph.addMonadicEdges $ Graph.mkGraph
        [(-2, Node.Inputs         (0, 0))
        , fixEmpty' (100, Node.Expr "a.foo bar baz (gaz 1 \"asda\")" "" (0, 1))
        ,(-3, Node.Outputs        (0, 2))
        ]
        []
     , named "BATCH-62"
     $ Graph.addMonadicEdges $ Graph.mkGraph
        [(-2, Node.Inputs         (0, 0))
        , fixEmpty' (100, Node.Expr "12" "" (0, 1))
        ,(-3, Node.Outputs        (0, 2))
        ]
        [(100, -3, Edge.Data Port.All $ Port.Num 1)]
     , named "BATCH-67"
     $ Graph.addMonadicEdges $ Graph.mkGraph
        [(-2, Node.Inputs         (0, 0))
        , fixEmpty' (100, Node.Expr "12" "" (0, 1))
        , fixEmpty' (200, Node.Expr "15" "" (0, 1))
        , fixEmpty' (300, Node.Expr "*" "" (0, 1))
        , fixEmpty' (400, Node.Expr "+" "" (0, 1))
        ,(-3, Node.Outputs        (0, 2))
        ]
        [(100, 300, Edge.Data Port.All $ Port.Num 0)
        ,(200, 300, Edge.Data Port.All $ Port.Num 1)
        ,(300, 400, Edge.Data Port.All $ Port.Num 0)
        ,(300, 400, Edge.Data Port.All $ Port.Num 1)
        ]
     , named "graph with [] port descriptor on output"
     $ Graph.addMonadicEdges $ Graph.mkGraph
        [(-2,Node.Inputs  (0  ,0))
        ,(-3,Node.Outputs (100,0))
        ,fixEmpty' (100, Node.Expr "foo" "" (0 , 1))
        ]
        [(100, -3, Edge.Data Port.All   Port.All)]
     , named "graph with [0] port descriptor on output"
     $ Graph.addMonadicEdges $ Graph.mkGraph
        [(-2,Node.Inputs  (0  ,0))
        ,(-3,Node.Outputs (100,0))
        ,fixEmpty' (100, Node.Expr "foo" "" (0 , 1))
        ]
        [(100, -3, Edge.Data Port.All $ Port.Num 0)]
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
    ),( "graph with [0] and [1] port descriptors on output"
     ,  Graph.addMonadicEdges $ Graph.mkGraph
        [(-2,Node.Inputs  (0 ,0))
        ,(-3,Node.Outputs (100,0))
        ,fixEmpty' (100, Node.Expr "foo" "" (0 , 1))
        ]
        [(100, -3, Edge.Data Port.All $ Port.Num 0)
        ,(100, -3, Edge.Data Port.All $ Port.Num 1)
        ]
     ,  Graph.addMonadicEdges $ Graph.mkGraph
        [(-2,Node.Inputs  (0 ,0))
        ,(-3,Node.Outputs (100,0))
        ,fixEmpty' (100, Node.Expr "foo" "" (0 , 1))
        ]
        [(100, -3, Edge.Data Port.All $ Port.Num 0)
        ,(100, -3, Edge.Data Port.All $ Port.Num 1)
        ]
    ),( "graph with [] and [1] port descriptors on output - 1"
     ,  Graph.addMonadicEdges $ Graph.mkGraph
        [(-2,Node.Inputs  (0 ,0))
        ,(-3,Node.Outputs (100,0))
        ,fixEmpty' (100, Node.Expr "foo" "" (0 , 1))
        ]
        [(100, -3, Edge.Data Port.All $ Port.All)
        ,(100, -3, Edge.Data Port.All $ Port.Num 1)
        ]
     ,  Graph.addMonadicEdges $ Graph.mkGraph
        [(-2,Node.Inputs  (0 ,0))
        ,(-3,Node.Outputs (100,0))
        ,fixEmpty' (100, Node.Expr "foo" "" (0 , 1))
        ]
        [(100, -3, Edge.Data Port.All $ Port.Num 0)
        ,(100, -3, Edge.Data Port.All $ Port.Num 1)
        ]
    ),( "graph with [] and [1] port descriptors on output - 2"
     ,  Graph.addMonadicEdges $ Graph.mkGraph
        [(-2,Node.Inputs  (0 ,0))
        ,(-3,Node.Outputs (100,0))
        ,fixEmpty' (100, Node.Expr "foo" "" (0 , 1))
        ,fixEmpty' (200, Node.Expr "bar" "" (0 , 2))
        ]
        [(100, -3, Edge.Data Port.All $ Port.All)
        ,(200, -3, Edge.Data Port.All $ Port.Num 1)
        ]
     ,  Graph.addMonadicEdges $ Graph.mkGraph
        [(-2,Node.Inputs  (0 ,0))
        ,(-3,Node.Outputs (100,0))
        ,fixEmpty' (100, Node.Expr "foo" "" (0 , 1))
        ,fixEmpty' (200, Node.Expr "bar" "" (0 , 2))
        ]
        [(100, -3, Edge.Data Port.All $ Port.Num 0)
        ,(200, -3, Edge.Data Port.All $ Port.Num 1)
        ]
    ),( "graph with [], [1] and [2] port descriptors on output - 1"
     ,  Graph.addMonadicEdges $ Graph.mkGraph
        [(-2,Node.Inputs  (0 ,0))
        ,(-3,Node.Outputs (100,0))
        ,fixEmpty' (100, Node.Expr "foo" "" (0 , 1))
        ]
        [(100, -3, Edge.Data Port.All $ Port.All)
        ,(100, -3, Edge.Data Port.All $ Port.Num 1)
        ,(100, -3, Edge.Data Port.All $ Port.Num 2)
        ]
     ,  Graph.addMonadicEdges $ Graph.mkGraph
        [(-2,Node.Inputs  (0 ,0))
        ,(-3,Node.Outputs (100,0))
        ,fixEmpty' (100, Node.Expr "foo" "" (0 , 1))
        ]
        [(100, -3, Edge.Data Port.All $ Port.Num 0)
        ,(100, -3, Edge.Data Port.All $ Port.Num 1)
        ,(100, -3, Edge.Data Port.All $ Port.Num 2)
        ]
    ),( "graph with [], [1] and [2] port descriptors on output - 2"
     ,  Graph.addMonadicEdges $ Graph.mkGraph
        [(-2,Node.Inputs  (0 ,0))
        ,(-3,Node.Outputs (100,0))
        ,fixEmpty' (100, Node.Expr "foo" "" (0 , 1))
        ,fixEmpty' (200, Node.Expr "bar" "" (0 , 2))
        ]
        [(100, -3, Edge.Data Port.All $ Port.All)
        ,(200, -3, Edge.Data Port.All $ Port.Num 1)
        ,(100, -3, Edge.Data Port.All $ Port.Num 2)
        ]
     ,  Graph.addMonadicEdges $ Graph.mkGraph
        [(-2,Node.Inputs  (0 ,0))
        ,(-3,Node.Outputs (100,0))
        ,fixEmpty' (100, Node.Expr "foo" "" (0 , 1))
        ,fixEmpty' (200, Node.Expr "bar" "" (0 , 2))
        ]
        [(100, -3, Edge.Data Port.All $ Port.Num 0)
        ,(200, -3, Edge.Data Port.All $ Port.Num 1)
        ,(100, -3, Edge.Data Port.All $ Port.Num 2)
        ]
    )]
