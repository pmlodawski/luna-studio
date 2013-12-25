---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------
{-# LANGUAGE ConstraintKinds  #-}
{-# LANGUAGE FlexibleContexts #-}

module Flowbox.Luna.Passes.Transform.Graph.Parser.Parser where

import Control.Monad.State

import           Flowbox.Luna.Data.AST.Expr                         (Expr)
import qualified Flowbox.Luna.Data.AST.Expr                         as Expr
import qualified Flowbox.Luna.Data.AST.Pat                          as Pat
import qualified Flowbox.Luna.Data.Attributes                       as Attributes
import           Flowbox.Luna.Data.Graph.Graph                      (Graph)
import qualified Flowbox.Luna.Data.Graph.Graph                      as Graph
import           Flowbox.Luna.Data.Graph.Node                       (Node)
import qualified Flowbox.Luna.Data.Graph.Node                       as Node
import qualified Flowbox.Luna.Data.Graph.Port                       as Port
import           Flowbox.Luna.Data.Graph.Properties                 (Properties)
import qualified Flowbox.Luna.Data.Graph.Properties                 as Properties
import           Flowbox.Luna.Passes.Pass                           (PassMonad)
import qualified Flowbox.Luna.Passes.Pass                           as Pass
import qualified Flowbox.Luna.Passes.Transform.AST.TxtParser.Parser as Parser
import qualified Flowbox.Luna.Passes.Transform.Graph.Attributes     as Attributes
import           Flowbox.Luna.Passes.Transform.Graph.Parser.State   (GPState)
import qualified Flowbox.Luna.Passes.Transform.Graph.Parser.State   as State
import           Flowbox.Prelude                                    hiding (error, folded, mapM, mapM_)
import           Flowbox.System.Log.Logger



logger :: Logger
logger = getLogger "Flowbox.Luna.Passes.Transform.Graph.Parser.Parser"


type GPMonad m = PassMonad GPState m


run :: PassMonad s m => Graph -> Expr -> Pass.Result m Expr
run gr = (Pass.run_ (Pass.Info "GraphParser") $ State.make gr) . graph2expr


graph2expr :: GPMonad m => Expr -> Pass.Result m Expr
graph2expr expr = do
    let inputs = expr ^. Expr.inputs
    graph <- State.getGraph
    mapM_ (parseNode inputs) $ Graph.topsortl graph
    body <- State.getBody
    return (Expr.body .~ body $ expr)


parseNode :: GPMonad m => [Expr] ->  (Node.ID, Node) -> Pass.Result m ()
parseNode inputs (nodeID, node) = do
    case node of
        Node.Expr expr _ properties -> parseExprNode    nodeID expr properties
        Node.Inputs  properties     -> parseInputsNode  nodeID inputs properties
        Node.Outputs properties     -> parseOutputsNode nodeID properties


parseExprNode :: GPMonad m => Node.ID -> String -> Properties -> Pass.Result m ()
parseExprNode nodeID expr properties = case expr of
    '=':pat -> parsePatNode   nodeID pat  properties
    '~':_   -> parseInfixNode nodeID expr properties
    _       -> parseAppNode   nodeID expr properties


parseInputsNode :: GPMonad m => Node.ID -> [Expr] -> Properties -> Pass.Result m ()
parseInputsNode nodeID inputs properties = do
    mapM_ (parseArg nodeID properties) $ zip [0..] inputs


parseArg :: State.GPStateM m => Node.ID -> Properties -> (Int, Expr) -> m ()
parseArg nodeID properties (num, input) = case input of
    Expr.Arg _ (Pat.Var _ name) _ -> State.addToNodeMap (nodeID, Port.Num num) $ Expr.Var unknownID name
    _ -> fail "parseArg: Wrong Arg type"


parseOutputsNode :: GPMonad m => Node.ID -> Properties -> Pass.Result m ()
parseOutputsNode nodeID properties = do
    return ()


parsePatNode :: GPMonad m => Node.ID -> String -> Properties -> Pass.Result m ()
parsePatNode nodeID pat properties = do
    srcs <- State.getNodeSrcs nodeID
    case srcs of
        [s] -> do p <- case Parser.parsePattern pat unknownID of
                            Left  er     -> fail $ show er
                            Right (p, _) -> return p
                  let e = Expr.Assignment nodeID p s
                  State.addToNodeMap (nodeID, Port.All) e
                  State.addToBody e
        _      -> fail "parsePatNode: Wrong Pat arguments"


parseInfixNode :: GPMonad m => Node.ID -> String -> Properties -> Pass.Result m ()
parseInfixNode nodeID inf properties = do
    srcs <- State.getNodeSrcs nodeID
    case srcs of
        [a, b] -> do let e = Expr.Infix nodeID inf a b
                     addExpr nodeID e
        _      -> fail "parseInfixNode: Wrong Infix arguments"


parseAppNode :: GPMonad m => Node.ID -> String -> Properties -> Pass.Result m ()
parseAppNode nodeID app properties = do
    srcs <- State.getNodeSrcs nodeID
    case srcs of
        []  -> case Parser.parseExpr app nodeID of
                    Left  er     -> fail $ show er
                    Right (e, _) -> addExpr nodeID e
        [f] -> do let e   = Expr.Accessor nodeID app f
                  addExpr nodeID e
        f:t -> do let acc = Expr.Accessor nodeID app f
                      e   = Expr.App      unknownID acc t
                  addExpr nodeID e


addExpr :: GPMonad m => Node.ID -> Expr -> Pass.Result m ()
addExpr nodeID e = do
    folded <- isFolded nodeID
    if folded
        then State.addToNodeMap (nodeID, Port.All) e
        else do outName <- State.getNodeOutputName nodeID
                let p = Pat.Var unknownID outName
                    v = Expr.Var unknownID outName
                    a = Expr.Assignment unknownID p e
                State.addToNodeMap (nodeID, Port.All) v
                State.addToBody a


isFolded :: GPMonad m => Node.ID -> Pass.Result m Bool
isFolded nodeID = do
    node <- State.getNode nodeID
    let attrs = node ^. (Node.properties . Properties.attrs)
    case Attributes.get Attributes.luna Attributes.astFolded attrs of
        Just "True" -> return True
        _           -> return False

unknownID = (-1)