---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------

module Test.Luna.DEP.Sample.GraphView where

import           Flowbox.Prelude
import qualified Luna.DEP.Graph.Node                       as Node
import           Luna.DEP.Graph.Node.Expr                  (NodeExpr)
import qualified Luna.DEP.Graph.Node.Expr                  as NodeExpr
import qualified Luna.DEP.Graph.Node.StringExpr            as StringExpr
import           Luna.DEP.Graph.View.EdgeView              (EdgeView (EdgeView))
import           Luna.DEP.Graph.View.GraphView             (GraphView)
import qualified Luna.DEP.Graph.View.GraphView             as GraphView
import           Test.Luna.DEP.Pass.Transform.Graph.Common (named)



strExpr :: String -> NodeExpr
strExpr = NodeExpr.StringExpr . StringExpr.Expr


sampleGraphs :: [(String, GraphView)]
sampleGraphs =
    [ named "simple graphview"
    $ GraphView.mkGraph
        [(-1, Node.Inputs  (0, 0))
        ,(-2, Node.Outputs (0, 0))
        ,(0 , Node.Expr (strExpr "foo") "" (0, 0))
        ,(1 , Node.Expr (strExpr "bar") "" (1, 1))
        ,(2 , Node.Expr (strExpr "baz") "" (0, 0))
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
        ,(0 , Node.Expr (strExpr "foo") "" (0, 0))
        ,(1 , Node.Expr (strExpr "bar") "" (1, 1))
        ,(2 , Node.Expr (strExpr "baz") "" (0, 0))
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
        ,(0 , Node.Expr (strExpr "foo") "" (0, 0))
        ,(1 , Node.Expr (strExpr "bar") "" (1, 1))
        ,(2 , Node.Expr (strExpr "baz") "" (0, 0))
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
        ,(0 , Node.Expr (strExpr "foo") "" (0, 0))
        ,(1 , Node.Expr (strExpr "bar") "" (1, 1))
        ,(2 , Node.Expr (strExpr "baz") "" (0, 0))
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
        ,(0 , Node.Expr (strExpr "foo") "" (0, 0))
        ]
        [(0 , -2, EdgeView [0] [1])
        ]
    , named "graphview with single-value output port descriptor 2"
    $ GraphView.mkGraph
        [(-1, Node.Inputs  (0, 0))
        ,(-2, Node.Outputs (0, 0))
        ,(0 , Node.Expr (strExpr "foo") "" (0, 0))
        ]
        [(0 , -2, EdgeView [0] [0])
        ]
    , named "graphview with empty output port descriptor 1"
    $ GraphView.mkGraph
        [(-1, Node.Inputs  (0, 0))
        ,(-2, Node.Outputs (0, 0))
        ,(0 , Node.Expr (strExpr "foo") "" (0, 0))
        ]
        [(0 , -2, EdgeView [0] [])
        ]
    , named "graphview with empty output port descriptor 2"
    $ GraphView.mkGraph
        [(-1, Node.Inputs  (0, 0))
        ,(-2, Node.Outputs (0, 0))
        ,(0 , Node.Expr (strExpr "foo") "" (0, 0))
        ]
        [(-1 , 0, EdgeView [0] [])
        ,(0 , -2, EdgeView [0] [])
        ]
    ]
