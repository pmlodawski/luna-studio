---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------

module Luna.Syntax.Graph.Node.OutputName (
    provide,
    fixEmpty,
    fixEmpty',
    generate,
) where

import qualified Data.Char as Char
import qualified Data.List as List

import           Flowbox.Prelude
import           Luna.Syntax.Graph.Node            (Node)
import qualified Luna.Syntax.Graph.Node            as Node
import           Luna.Syntax.Graph.Node.Expr       (NodeExpr)
import qualified Luna.Syntax.Graph.Node.Expr       as NodeExpr
import qualified Luna.Syntax.Graph.Node.StringExpr as StringExpr
import           Luna.Syntax.Label                 (Label (Label))
import           Luna.Syntax.Name                  (VNameP)
import           Luna.Syntax.Pat                   (LPat)
import qualified Luna.Syntax.Pat                   as Pat



generate :: Default a => NodeExpr a e -> Int -> LPat a
generate nodeExpr num = Label def $ Pat.Var $ fromString $ mangle (exprStr ++ "Result") ++ show num where
    exprStr = case nodeExpr of
        NodeExpr.ASTExpr    {}      -> ""
        NodeExpr.StringExpr strExpr -> StringExpr.toString strExpr


fixEmpty :: Default a => Node a e -> Node.ID -> Node a e
fixEmpty node nodeID = case Node.getOutputName node of
    Nothing -> provide node nodeID
    _       -> node


fixEmpty' :: Default a => (Node.ID, Node a e) -> (Node.ID, Node a e)
fixEmpty' (nodeID, node) =
    (nodeID, fixEmpty node nodeID)


provide :: Default a => Node a e -> Node.ID -> Node a e
provide node@(Node.Expr nodeExpr _ _ _) nodeID =
    node & Node.outputName .~ Just (generate nodeExpr nodeID)
provide node _ = node


mangle :: String -> String
mangle name = case List.takeWhile Char.isAlphaNum name of
    f:alphaNum -> if Char.isDigit f
                     then 'r' : f : alphaNum
                     else Char.toLower f : alphaNum
    []         -> "node"
