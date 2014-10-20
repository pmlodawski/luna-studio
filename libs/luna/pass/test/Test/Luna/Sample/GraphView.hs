---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------

module Test.Luna.Sample.GraphView where

import           Flowbox.Prelude
import qualified Luna.Graph.Node                       as Node
import qualified Luna.Graph.Node.Expr                  as NodeExpr
import           Luna.Graph.View.EdgeView              (EdgeView (EdgeView))
import           Luna.Graph.View.GraphView             (GraphView)
import qualified Luna.Graph.View.GraphView             as GraphView
import           Test.Luna.Pass.Transform.Graph.Common (named)



sampleGraphs :: [(String, GraphView)]
sampleGraphs =
    [ named "simple graphview"
    $ GraphView.mkGraph
        [(-1, Node.Inputs  (0, 0))
        ,(-2, Node.Outputs (0, 0))
        ,(0 , Node.Expr (NodeExpr.Expr "foo") "" (0, 0))
        ,(1 , Node.Expr (NodeExpr.Expr "bar") "" (1, 1))
        ,(2 , Node.Expr (NodeExpr.Expr "baz") "" (0, 0))
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
        ,(0 , Node.Expr (NodeExpr.Expr "foo") "" (0, 0))
        ,(1 , Node.Expr (NodeExpr.Expr "bar") "" (1, 1))
        ,(2 , Node.Expr (NodeExpr.Expr "baz") "" (0, 0))
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
        ,(0 , Node.Expr (NodeExpr.Expr "foo") "" (0, 0))
        ,(1 , Node.Expr (NodeExpr.Expr "bar") "" (1, 1))
        ,(2 , Node.Expr (NodeExpr.Expr "baz") "" (0, 0))
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
        ,(0 , Node.Expr (NodeExpr.Expr "foo") "" (0, 0))
        ,(1 , Node.Expr (NodeExpr.Expr "bar") "" (1, 1))
        ,(2 , Node.Expr (NodeExpr.Expr "baz") "" (0, 0))
        ]
        [(0, 1, EdgeView [0, 2]    [1])
        ,(0, 1, EdgeView [1, 2, 3] [2])
        ,(1, 2, EdgeView []        [0, 1, 2])
        ,(1, 2, EdgeView [1, 3, 4] [3, 5, 7, 9])
        ]
    , named "graphview with single-value output port descriptor 1"
    $ GraphView.mkGraph
        [(-1, Node.Inputs  (0, 0))
        ,(-2, Node.Outputs (0, 0))
        ,(0 , Node.Expr (NodeExpr.Expr "foo") "" (0, 0))
        ]
        [(0 , -2, EdgeView [0] [1])
        ]
    , named "graphview with single-value output port descriptor 2"
    $ GraphView.mkGraph
        [(-1, Node.Inputs  (0, 0))
        ,(-2, Node.Outputs (0, 0))
        ,(0 , Node.Expr (NodeExpr.Expr "foo") "" (0, 0))
        ]
        [(0 , -2, EdgeView [0] [0])
        ]
    , named "graphview with empty output port descriptor 1"
    $ GraphView.mkGraph
        [(-1, Node.Inputs  (0, 0))
        ,(-2, Node.Outputs (0, 0))
        ,(0 , Node.Expr (NodeExpr.Expr "foo") "" (0, 0))
        ]
        [(0 , -2, EdgeView [0] [])
        ]
    , named "graphview with empty output port descriptor 2"
    $ GraphView.mkGraph
        [(-1, Node.Inputs  (0, 0))
        ,(-2, Node.Outputs (0, 0))
        ,(0 , Node.Expr (NodeExpr.Expr "foo") "" (0, 0))
        ]
        [(-1 , 0, EdgeView [0] [])
        ,(0 , -2, EdgeView [0] [])
        ]
    ]
