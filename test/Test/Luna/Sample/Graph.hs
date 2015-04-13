---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------

module Test.Luna.Sample.Graph where

import Control.Monad.State

import           Flowbox.Prelude
import           Luna.Data.ASTInfo                     (ASTInfo (ASTInfo))
import qualified Luna.Syntax.Graph.Edge                as Edge
import           Luna.Syntax.Graph.Graph               (Graph)
import qualified Luna.Syntax.Graph.Graph               as Graph
import           Luna.Syntax.Graph.Node                (Node)
import qualified Luna.Syntax.Graph.Node                as Node
import           Luna.Syntax.Graph.Node.Expr           (NodeExpr)
import qualified Luna.Syntax.Graph.Node.Expr           as NodeExpr
import qualified Luna.Syntax.Graph.Node.OutputPat      as OutputPat
import qualified Luna.Syntax.Graph.Node.StringExpr     as StringExpr
import qualified Luna.Syntax.Graph.Port                as Port
import           Luna.Syntax.Graph.Tag                 (Tag)
import           Test.Luna.Pass.Transform.Graph.Common (named)



type V = ()


strExpr :: String -> NodeExpr Tag V
strExpr = NodeExpr.StringExpr . StringExpr.Expr


fixEmpty :: Int -> (Node.ID, Node Tag V) -> (Node.ID, Node Tag V)
fixEmpty start = flip evalState (ASTInfo start) . OutputPat.fixEmpty'


samples :: [(String, Graph Tag V)]
samples =
    [ named "simple graph 1"
    $ Graph.addMonadicEdges $ Graph.mkGraph
        [(-2, Node.Inputs         (0, 0))
        , fixEmpty 10 (100, Node.Expr (strExpr "foo") def def (0, 1) def False)
        ,(-1, Node.Outputs def    (0, 2))
        ]
        []
    , named "simple graph 2"
    $ Graph.addMonadicEdges $ Graph.mkGraph
        [(-2, Node.Inputs         (0, 0))
        , fixEmpty 10 (100, Node.Expr (strExpr "main") def def (0, 1) def False)
        ,(-1, Node.Outputs def    (0, 2))
        ]
        []
    , named "simple graph 3"
    $ Graph.addMonadicEdges $ Graph.mkGraph
        [(-2, Node.Inputs         (0, 0))
        , fixEmpty 10 (100, Node.Expr (strExpr "foo") def def (0, 1) def False)
        , fixEmpty 10 (200, Node.Expr (strExpr "bar") def def (0, 2) def False)
        ,(-1, Node.Outputs def    (0, 3))
        ]
        [(100, 200, Edge.Data Port.mkSrcAll $ Port.mkDst 5)]
    , named "simple graph 4"
    $ Graph.addMonadicEdges $ Graph.mkGraph
        [(-2, Node.Inputs         (0, 0))
        , fixEmpty 10 (100, Node.Expr (strExpr "foo") def def (0, 1) def False)
        , fixEmpty 10 (200, Node.Expr (strExpr "bar") def def (0, 2) def False)
        ,(-1, Node.Outputs def    (0, 3))
        ]
        [(100, 200, Edge.Data Port.mkSrcAll $ Port.mkDst 5)
        ,(100, 200, Edge.Data Port.mkSrcAll $ Port.mkDst 3)
        ]
    , named "simple graph 5"
    $ Graph.addMonadicEdges $ Graph.mkGraph
        [(-2, Node.Inputs         (0, 0))
        , fixEmpty 10 (100, Node.Expr (strExpr "foo") def def (0, 1) def False)
        ,(-1, Node.Outputs def    (0, 3))
        ]
        [(100, -1, Edge.Data Port.mkSrcAll $ Port.mkDst 2)
        ,(100, -1, Edge.Data Port.mkSrcAll $ Port.mkDst 3)
        ]
    , named "simple graph 6"
    $ Graph.addMonadicEdges $ Graph.mkGraph
        [(-2, Node.Inputs         (0, 0))
        , fixEmpty 10 (100, Node.Expr (strExpr "main") def def (0, 1) def False)
        ,(-1, Node.Outputs def    (0, 3))
        ]
        [(100, -1, Edge.Data Port.mkSrcAll Port.mkDstAll)]
    , named "simple graph 7"
    $ Graph.addMonadicEdges $ Graph.mkGraph
        [(-2, Node.Inputs         (0, 0))
        , fixEmpty 10 (100, Node.Expr (strExpr "main") def def (0, 1) def False)
        ,(-1, Node.Outputs def    (0, 3))
        ]
        [(-2 ,100, Edge.Data (Port.mkSrc 0) $ Port.mkDst 0)
        ,(100, -1, Edge.Data  Port.mkSrcAll   Port.mkDstAll  )
        ]
    , named "simple graph 8"
    $ Graph.addMonadicEdges $ Graph.mkGraph
        [(-2, Node.Inputs         (0, 0))
        , fixEmpty 10 (100, Node.Expr (strExpr "25") def def (0, 1) def False)
        , fixEmpty 20 (200, Node.Expr (strExpr "*") def def (0, 1) def False)
        ,(-1, Node.Outputs def    (0, 3))
        ]
        [(100,200, Edge.Data Port.mkSrcAll $ Port.mkDst 0)
        ,(100,200, Edge.Data Port.mkSrcAll $ Port.mkDst 1)
        ]
    , named "inverse order graph"
    $ Graph.addMonadicEdges $ Graph.mkGraph
        [(-2, Node.Inputs         (0, 0))
        , fixEmpty 10 (100, Node.Expr (strExpr "foo") def def (0, 1) def False)
        , fixEmpty 20 (200, Node.Expr (strExpr "bar") def def (0, 2) def False)
        ,(-1, Node.Outputs def    (0, 3))
        ]
        [(200, 100, Edge.Data Port.mkSrcAll $ Port.mkDst 5)]
    -- , named "graph with folded nodes 1"
    -- $ Graph.addMonadicEdges $ Graph.mkGraph
    --    [(-2, Node.Inputs         (0, 0))
    --    , fixEmpty 10 (100, Node.Expr "1 * 2 * 3" "" (0, 1))
    --    ,(-1, Node.Outputs def    (0, 2))
    --    ]
    --    []
    , named "graph with folded nodes 2"
    $ Graph.addMonadicEdges $ Graph.mkGraph
        [(-2, Node.Inputs         (0, 0))
        , fixEmpty 10 (100, Node.Expr (strExpr "1.+ 2") def def (0, 1) def False)
        ,(-1, Node.Outputs def    (0, 2))
        ]
        []
    , named "graph with folded nodes 3"
    $ Graph.addMonadicEdges $ Graph.mkGraph
        [(-2, Node.Inputs         (0, 0))
        , fixEmpty 10 (100, Node.Expr (strExpr "1.+ 2.* 2") def def (0, 1) def False)
        ,(-1, Node.Outputs def    (0, 2))
        ]
        []
    , named "graph with folded nodes 4"
    $ Graph.addMonadicEdges $ Graph.mkGraph
        [(-2, Node.Inputs         (0, 0))
        , fixEmpty 10 (100, Node.Expr (strExpr "(1.+ 2).* 2") def def (0, 1) def False)
        ,(-1, Node.Outputs def    (0, 2))
        ]
        []
    , named "graph with folded nodes 5"
    $ Graph.addMonadicEdges $ Graph.mkGraph
        [(-2, Node.Inputs         (0, 0))
        , fixEmpty 10 (100, Node.Expr (strExpr "a.foo bar 1 \"asda\"") def def (0, 1) def False)
        ,(-1, Node.Outputs def    (0, 2))
        ]
        []
    , named "graph with folded nodes 6"
    $ Graph.addMonadicEdges $ Graph.mkGraph
        [(-2, Node.Inputs         (0, 0))
        , fixEmpty 10 (100, Node.Expr (strExpr "a.foo bar baz (gaz 1 \"asda\")") def def (0, 1) def False)
        ,(-1, Node.Outputs def    (0, 2))
        ]
        []
    , named "BATCH-62"
    $ Graph.addMonadicEdges $ Graph.mkGraph
        [(-2, Node.Inputs         (0, 0))
        , fixEmpty 10 (100, Node.Expr (strExpr "12") def def (0, 1) def False)
        ,(-1, Node.Outputs def    (0, 2))
        ]
        [(100, -1, Edge.Data Port.mkSrcAll $ Port.mkDst 1)]
    , named "BATCH-67"
    $ Graph.addMonadicEdges $ Graph.mkGraph
        [(-2, Node.Inputs         (0, 0))
        , fixEmpty 10 (100, Node.Expr (strExpr "12") def def (0, 1) def False)
        , fixEmpty 20 (200, Node.Expr (strExpr "15") def def (0, 1) def False)
        , fixEmpty 30 (300, Node.Expr (strExpr "*") def def (0, 1) def False)
        , fixEmpty 40 (400, Node.Expr (strExpr "+") def def (0, 1) def False)
        ,(-1, Node.Outputs def    (0, 2))
        ]
        [(100, 300, Edge.Data Port.mkSrcAll $ Port.mkDst 0)
        ,(200, 300, Edge.Data Port.mkSrcAll $ Port.mkDst 1)
        ,(300, 400, Edge.Data Port.mkSrcAll $ Port.mkDst 0)
        ,(300, 400, Edge.Data Port.mkSrcAll $ Port.mkDst 1)
        ]
    , named "graph with [] port descriptor on output"
    $ Graph.addMonadicEdges $ Graph.mkGraph
        [(-2,Node.Inputs  (0  ,0))
        ,(-1,Node.Outputs def (100,0))
        ,fixEmpty 10 (100, Node.Expr (strExpr "foo") def def (0, 1) def False)
        ]
        [(100, -1, Edge.Data Port.mkSrcAll   Port.mkDstAll)]
    , named "graph with [0] port descriptor on output - 1"
    $ Graph.addMonadicEdges $ Graph.mkGraph
        [(-2,Node.Inputs  (0 ,0))
        ,(-1,Node.Outputs def (10,0))
        ,fixEmpty 10 (100, Node.Expr (strExpr "foo") def def (0, 1) def False)
        ]
        [(100, -1, Edge.Data Port.mkSrcAll $ Port.mkDst 0)]
    , named "graph with [0] port descriptor on output - 2"
    $ Graph.addMonadicEdges $ Graph.mkGraph
        [(-2,Node.Inputs  (0 ,0))
        ,(-1,Node.Outputs def (10,0))
        ,fixEmpty 10 (100, Node.Expr (strExpr "255") def def (0, 1) def False)
        ]
        [(100, -1, Edge.Data Port.mkSrcAll $ Port.mkDst 0)]
    ]


buggy :: [(String, Graph Tag V, Graph Tag V)]
buggy =
    [(  "empty"
     ,  Graph.addMonadicEdges $ Graph.mkGraph
        []
        []
     ,  Graph.addMonadicEdges $ Graph.mkGraph
        [(-2,Node.Inputs  (0 ,0))
        ,(-1,Node.Outputs def (10,0))
        ]
        []
    ),( "buggy graph 1"
     ,  Graph.mkGraph
        [fixEmpty 10 (100, Node.Expr (strExpr "main") def def (0, 1) def False)]
        []
     ,  Graph.addMonadicEdges $ Graph.mkGraph
        [(-2,Node.Inputs           (0, 0))
        ,(-1,Node.Outputs def      (10, 1))
        ,fixEmpty 10 (100, Node.Expr (strExpr "main") def def (0, 1) def False)
        ]
        []
    ),( "graph with [0] and [1] port descriptors on output"
     ,  Graph.addMonadicEdges $ Graph.mkGraph
        [(-2,Node.Inputs  (0 ,0))
        ,(-1,Node.Outputs def (100,0))
        ,fixEmpty 10 (100, Node.Expr (strExpr "foo") def def (0, 1) def False)
        ]
        [(100, -1, Edge.Data Port.mkSrcAll $ Port.mkDst 0)
        ,(100, -1, Edge.Data Port.mkSrcAll $ Port.mkDst 1)
        ]
     ,  Graph.addMonadicEdges $ Graph.mkGraph
        [(-2,Node.Inputs  (0 ,0))
        ,(-1,Node.Outputs def (100,0))
        ,fixEmpty 10 (100, Node.Expr (strExpr "foo") def def (0, 1) def False)
        ]
        [(100, -1, Edge.Data Port.mkSrcAll $ Port.mkDst 0)
        ,(100, -1, Edge.Data Port.mkSrcAll $ Port.mkDst 1)
        ]
    ),( "graph with [] and [1] port descriptors on output - 1"
     ,  Graph.addMonadicEdges $ Graph.mkGraph
        [(-2,Node.Inputs  (0 ,0))
        ,(-1,Node.Outputs def (100,0))
        ,fixEmpty 10 (100, Node.Expr (strExpr "foo") def def (0, 1) def False)
        ]
        [(100, -1, Edge.Data Port.mkSrcAll $ Port.mkDstAll)
        ,(100, -1, Edge.Data Port.mkSrcAll $ Port.mkDst 1)
        ]
     ,  Graph.addMonadicEdges $ Graph.mkGraph
        [(-2,Node.Inputs  (0 ,0))
        ,(-1,Node.Outputs def (100,0))
        ,fixEmpty 10 (100, Node.Expr (strExpr "foo") def def (0, 1) def False)
        ]
        [(100, -1, Edge.Data Port.mkSrcAll $ Port.mkDst 0)
        ,(100, -1, Edge.Data Port.mkSrcAll $ Port.mkDst 1)
        ]
    ),( "graph with [] and [1] port descriptors on output - 2"
     ,  Graph.addMonadicEdges $ Graph.mkGraph
        [(-2,Node.Inputs  (0 ,0))
        ,(-1,Node.Outputs def (100,0))
        ,fixEmpty 10 (100, Node.Expr (strExpr "foo") def def (0, 1) def False)
        ,fixEmpty 20 (200, Node.Expr (strExpr "bar") def def (0, 2) def False)
        ]
        [(100, -1, Edge.Data Port.mkSrcAll $ Port.mkDstAll)
        ,(200, -1, Edge.Data Port.mkSrcAll $ Port.mkDst 1)
        ]
     ,  Graph.addMonadicEdges $ Graph.mkGraph
        [(-2,Node.Inputs  (0 ,0))
        ,(-1,Node.Outputs def (100,0))
        ,fixEmpty 10 (100, Node.Expr (strExpr "foo") def def (0, 1) def False)
        ,fixEmpty 20 (200, Node.Expr (strExpr "bar") def def (0, 2) def False)
        ]
        [(100, -1, Edge.Data Port.mkSrcAll $ Port.mkDst 0)
        ,(200, -1, Edge.Data Port.mkSrcAll $ Port.mkDst 1)
        ]
    ),( "graph with [], [1] and [2] port descriptors on output - 1"
     ,  Graph.addMonadicEdges $ Graph.mkGraph
        [(-2,Node.Inputs  (0 ,0))
        ,(-1,Node.Outputs def (100,0))
        ,fixEmpty 10 (100, Node.Expr (strExpr "foo") def def (0, 1) def False)
        ]
        [(100, -1, Edge.Data Port.mkSrcAll $ Port.mkDstAll)
        ,(100, -1, Edge.Data Port.mkSrcAll $ Port.mkDst 1)
        ,(100, -1, Edge.Data Port.mkSrcAll $ Port.mkDst 2)
        ]
     ,  Graph.addMonadicEdges $ Graph.mkGraph
        [(-2,Node.Inputs  (0 ,0))
        ,(-1,Node.Outputs def (100,0))
        ,fixEmpty 10 (100, Node.Expr (strExpr "foo") def def (0, 1) def False)
        ]
        [(100, -1, Edge.Data Port.mkSrcAll $ Port.mkDst 0)
        ,(100, -1, Edge.Data Port.mkSrcAll $ Port.mkDst 1)
        ,(100, -1, Edge.Data Port.mkSrcAll $ Port.mkDst 2)
        ]
    ),( "graph with [], [1] and [2] port descriptors on output - 2"
     ,  Graph.addMonadicEdges $ Graph.mkGraph
        [(-2,Node.Inputs  (0 ,0))
        ,(-1,Node.Outputs def (100,0))
        ,fixEmpty 10 (100, Node.Expr (strExpr "foo") def def (0, 1) def False)
        ,fixEmpty 20 (200, Node.Expr (strExpr "bar") def def (0, 2) def False)
        ]
        [(100, -1, Edge.Data Port.mkSrcAll $ Port.mkDstAll)
        ,(200, -1, Edge.Data Port.mkSrcAll $ Port.mkDst 1)
        ,(100, -1, Edge.Data Port.mkSrcAll $ Port.mkDst 2)
        ]
     ,  Graph.addMonadicEdges $ Graph.mkGraph
        [(-2,Node.Inputs  (0 ,0))
        ,(-1,Node.Outputs def (100,0))
        ,fixEmpty 10 (100, Node.Expr (strExpr "foo") def def (0, 1) def False)
        ,fixEmpty 20 (200, Node.Expr (strExpr "bar") def def (0, 2) def False)
        ]
        [(100, -1, Edge.Data Port.mkSrcAll $ Port.mkDst 0)
        ,(200, -1, Edge.Data Port.mkSrcAll $ Port.mkDst 1)
        ,(100, -1, Edge.Data Port.mkSrcAll $ Port.mkDst 2)
        ]
    )]
