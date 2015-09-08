---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------

module Test.Luna.DEP.Sample.NodeDefault where

import           Flowbox.Prelude
import qualified Luna.DEP.AST.Arg                          as Arg
import qualified Luna.DEP.AST.Expr                         as Expr
import qualified Luna.DEP.Graph.Node                       as Node
import           Luna.DEP.Graph.Node.Expr                  (NodeExpr)
import qualified Luna.DEP.Graph.Node.Expr                  as NodeExpr
import           Luna.DEP.Graph.Node.OutputName            (fixEmpty')
import qualified Luna.DEP.Graph.Node.StringExpr            as StringExpr
import           Luna.DEP.Graph.PropertyMap                (PropertyMap)
import qualified Luna.DEP.Graph.PropertyMap                as PropertyMap
import qualified Luna.DEP.Graph.View.Default.DefaultsMap   as DefaultsMap
import           Luna.DEP.Graph.View.GraphView             (GraphView)
import qualified Luna.DEP.Graph.View.GraphView             as GraphView
import           Test.Luna.DEP.Pass.Transform.Graph.Common (named3)



strExpr :: String -> NodeExpr
strExpr = NodeExpr.StringExpr . StringExpr.Expr


sampleGraphs :: [(String, GraphView, PropertyMap)]
sampleGraphs =
    [ named3 "string default"
        ( GraphView.mkGraph
            [(-2, Node.Inputs  (0, 0))
            ,(-3, Node.Outputs (10, 0))
            ,fixEmpty' (5 , Node.Expr (strExpr "foo") "" (0, 0))
            ]
            []
        )
        $ PropertyMap.modifyDefaultsMap
            (DefaultsMap.insert [0] (6, strExpr "\"some text\"")) 5 def
    , named3 "string default 2"
        ( GraphView.mkGraph
            [(-2, Node.Inputs  (0, 0))
            ,(-3, Node.Outputs (10, 0))
            ,fixEmpty' (5 , Node.Expr (strExpr "foo") "" (0, 0))
            ]
            []
        )
        $ PropertyMap.modifyDefaultsMap
            (DefaultsMap.insert [0] (6, strExpr "45")) 5 def
    , named3 "string default 3"
        ( GraphView.mkGraph
            [(-2, Node.Inputs  (0, 0))
            ,(-3, Node.Outputs (10, 0))
            ,fixEmpty' (5 , Node.Expr (strExpr "foo") "" (0, 0))
            ]
            []
        )
        $ PropertyMap.modifyDefaultsMap
            (DefaultsMap.insert [0] (6, strExpr "foo.bar.baz")) 5 def
    , named3 "ast default"
        ( GraphView.mkGraph
            [(-2, Node.Inputs  (0, 0))
            ,(-3, Node.Outputs (10, 0))
            ,fixEmpty' (5 , Node.Expr (strExpr "foo") "" (0, 0))
            ]
            []
        )
         $ PropertyMap.modifyDefaultsMap
            (DefaultsMap.insert [0] (500, NodeExpr.ASTExpr ast1)) 5 def
    ] where
        ast1 = Expr.App 0 (Expr.Accessor 0 (Expr.mkAccessor "foo")
                                           $ Expr.Con 0 "Some")
                          [Arg.Unnamed 0 $ Expr.List 0 []]
