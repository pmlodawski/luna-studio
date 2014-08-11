---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE ConstraintKinds  #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE Rank2Types       #-}

module Flowbox.Luna.Passes.Transform.Graph.Parser.Parser where

import Control.Monad.State
import Control.Monad.Trans.Either

import           Flowbox.Luna.Data.AST.Expr                         (Expr)
import qualified Flowbox.Luna.Data.AST.Expr                         as Expr
import qualified Flowbox.Luna.Data.AST.Pat                          as Pat
import           Flowbox.Luna.Data.Graph.Graph                      (Graph)
import qualified Flowbox.Luna.Data.Graph.Graph                      as Graph
import           Flowbox.Luna.Data.Graph.Node                       (Node)
import qualified Flowbox.Luna.Data.Graph.Node                       as Node
import qualified Flowbox.Luna.Data.Graph.Port                       as Port
import qualified Flowbox.Luna.Data.Pass.ASTInfo                     as ASTInfo
import           Flowbox.Luna.Data.PropertyMap                      (PropertyMap)
import qualified Flowbox.Luna.Passes.Pass                           as Pass
import qualified Flowbox.Luna.Passes.Transform.AST.IDFixer.State    as IDFixer
import qualified Flowbox.Luna.Passes.Transform.AST.TxtParser.Lexer  as Lexer
import qualified Flowbox.Luna.Passes.Transform.AST.TxtParser.Parser as Parser
import qualified Flowbox.Luna.Passes.Transform.Graph.Attributes     as Attributes
import           Flowbox.Luna.Passes.Transform.Graph.Parser.State   (GPPass)
import qualified Flowbox.Luna.Passes.Transform.Graph.Parser.State   as State
import           Flowbox.Prelude                                    hiding (error, folded, mapM, mapM_)
import           Flowbox.System.Log.Logger



logger :: Logger
logger = getLogger "Flowbox.Luna.Passes.Transform.Graph.Parser.Parser"


run :: Graph -> PropertyMap -> Expr -> Pass.Result Expr
run gr pm = (Pass.run_ (Pass.Info "GraphParser") $ State.make gr pm) . graph2expr


graph2expr :: Expr -> GPPass Expr
graph2expr expr = do
    let inputs = expr ^. Expr.inputs
    graph <- State.getGraph
    mapM_ (parseNode inputs) $ Graph.sort graph
    b  <- State.getBody
    mo <- State.getOutput
    let body = reverse $ case mo of
                Nothing -> b
                Just o  -> o : b
    return (expr & Expr.body .~ body)


parseNode :: [Expr] ->  (Node.ID, Node) -> GPPass ()
parseNode inputs (nodeID, node) = do
    case node of
        Node.Expr    {} -> parseExprNode    nodeID $ node ^. Node.expr
        Node.Inputs  {} -> parseInputsNode  nodeID inputs
        Node.Outputs {} -> parseOutputsNode nodeID
    State.setPosition nodeID $ node ^. Node.pos


parseExprNode :: Node.ID -> String -> GPPass ()
parseExprNode nodeID expr = case expr of
    "List"  -> parseListNode  nodeID
    "Tuple" -> parseTupleNode nodeID
    '=':pat -> parsePatNode   nodeID pat
    _       -> parseAppNode   nodeID expr


parseInputsNode :: Node.ID -> [Expr] -> GPPass ()
parseInputsNode nodeID = mapM_ (parseArg nodeID) . zip [0..]


parseArg :: Node.ID -> (Int, Expr) -> GPPass ()
parseArg nodeID (num, input) = case input of
    Expr.Arg _              (Pat.Var _ name)    _ -> State.addToNodeMap (nodeID, Port.Num num) $ Expr.Var IDFixer.unknownID name
    Expr.Arg _ (Pat.Typed _ (Pat.Var _ name) _) _ -> State.addToNodeMap (nodeID, Port.Num num) $ Expr.Var IDFixer.unknownID name
    _                                             -> left "parseArg: Wrong Arg type"


parseOutputsNode :: Node.ID -> GPPass ()
parseOutputsNode nodeID = do
    srcs <- State.getNodeSrcs nodeID
    when (length srcs > 1) $
        State.setOutput $ Expr.Tuple IDFixer.unknownID srcs


parsePatNode :: Node.ID -> String -> GPPass ()
parsePatNode nodeID pat = do
    srcs <- State.getNodeSrcs nodeID
    case srcs of
        [s] -> do p <- case Parser.parsePattern pat $ ASTInfo.mk IDFixer.unknownID of
                            Left  er     -> left $ show er
                            Right (p, _) -> return p
                  let e = Expr.Assignment nodeID p s
                  State.addToNodeMap (nodeID, Port.All) e
                  State.addToBody e
        _      -> left "parsePatNode: Wrong Pat arguments"


parseAppNode :: Node.ID -> String -> GPPass ()
parseAppNode nodeID app = do
    srcs <- State.getNodeSrcs nodeID
    expr <- if isOperator app
                then return $ Expr.Var nodeID app
                else case Parser.parseExpr app $ ASTInfo.mk nodeID of
                    Left  er     -> left $ show er
                    Right (e, _) -> return e
    let requiresApp (Expr.Con {}) = True
        requiresApp _             = False
    case srcs of
        []                   -> addExpr nodeID $ if requiresApp expr
                                    then Expr.App IDFixer.unknownID expr []
                                    else expr
        (Expr.Wildcard {}):t -> addExpr nodeID $ Expr.App IDFixer.unknownID expr t
        f:t                  -> do let acc = Expr.Accessor nodeID app f
                                       e   = Expr.App      IDFixer.unknownID acc t
                                   addExpr nodeID e


parseTupleNode :: Node.ID -> GPPass ()
parseTupleNode nodeID = do
    srcs <- State.getNodeSrcs nodeID
    let e = Expr.Tuple nodeID srcs
    addExpr nodeID e


parseListNode :: Node.ID -> GPPass ()
parseListNode nodeID = do
    srcs <- State.getNodeSrcs nodeID
    let e = Expr.List nodeID srcs
    addExpr nodeID e


addExpr :: Node.ID -> Expr -> GPPass ()
addExpr nodeID e = do
    folded         <- State.hasFlag nodeID Attributes.astFolded
    noAssignement  <- State.hasFlag nodeID Attributes.astNoAssignment
    defaultNodeGen <- State.hasFlag nodeID Attributes.defaultNodeGenerated

    if folded || defaultNodeGen
        then State.addToNodeMap (nodeID, Port.All) e
        else if noAssignement
            then do State.addToNodeMap (nodeID, Port.All) e
                    State.addToBody e
            else do outName <- State.getNodeOutputName nodeID
                    let p = Pat.Var IDFixer.unknownID outName
                        v = Expr.Var IDFixer.unknownID outName
                        a = Expr.Assignment IDFixer.unknownID p e
                    State.addToNodeMap (nodeID, Port.All) v
                    State.addToBody a


isOperator :: String -> Bool
isOperator expr = length expr == 1 && head expr `elem` Lexer.operators
