---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE TupleSections #-}

module Luna.Syntax.Graph.Node.OutputPat (
    provide,
    fixEmpty,
    fixEmpty',
    generate,
) where

import           Control.Monad.State
import qualified Data.Char                   as Char
import qualified Data.List                   as List

import           Flowbox.Prelude
import           Luna.Data.ASTInfo           (ASTInfo)
import qualified Luna.Data.ASTInfo           as ASTInfo
import           Luna.Syntax.Enum            (Enumerated)
import qualified Luna.Syntax.Enum            as Enum
import           Luna.Syntax.Graph.Node      (Node)
import qualified Luna.Syntax.Graph.Node      as Node
import           Luna.Syntax.Graph.Node.Expr (NodeExpr)
import qualified Luna.Syntax.Graph.Node.Expr as NodeExpr
import           Luna.Syntax.Label           (Label (Label))
import           Luna.Syntax.Pat             (LPat)
import qualified Luna.Syntax.Pat             as Pat



generate :: Enumerated a => NodeExpr a e -> Int -> State ASTInfo (LPat a)
generate nodeExpr num = newLabel $ Pat.Var $ fromString $ mangle (exprStr ++ "Result") ++ show num where
    exprStr = case nodeExpr of
        NodeExpr.ASTExpr    {}      -> ""
        NodeExpr.MultiPart  {}      -> ""
        NodeExpr.StringExpr strExpr -> toString strExpr


fixEmpty :: Enumerated a => Node a e -> Node.ID -> State ASTInfo (Node a e)
fixEmpty node nodeID = case Node.getOutputPat node of
    Nothing -> provide node nodeID
    _       -> return node


fixEmpty' :: Enumerated a => (Node.ID, Node a e) -> State ASTInfo (Node.ID, Node a e)
fixEmpty' (nodeID, node) = (nodeID,) <$> fixEmpty node nodeID


provide :: Enumerated a => Node a e -> Node.ID -> State ASTInfo (Node a e)
provide node@(Node.Expr nodeExpr _ _ _ _ _) nodeID = do
    outputPat <- generate nodeExpr nodeID
    return (node & Node.outputPat .~ Just outputPat)
provide node _ = return node


mangle :: String -> String
mangle name = case List.takeWhile Char.isAlphaNum name of
    f:alphaNum -> if Char.isDigit f
                     then 'r' : f : alphaNum
                     else Char.toLower f : alphaNum
    []         -> "node"


newLabel :: Enumerated a => e -> State ASTInfo (Label a e)
newLabel a = do
    n <- ASTInfo.incID <$> get
    put n
    return $ Label (Enum.tag $ n ^. ASTInfo.lastID) a
