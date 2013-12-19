---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}

module Flowbox.Luna.Passes.Transform.Graph.Parser.Parser where

import           Control.Applicative                                 
import           Control.Monad.State                                 
import qualified Data.List                                        as List
import qualified Data.Map                                         as Map
import           Data.Map                                           (Map)

import           Flowbox.Prelude                                  hiding (error, mapM, mapM_)
import           Flowbox.Luna.Data.AliasAnalysis                    (AA)
import qualified Flowbox.Luna.Data.AST.Expr                       as Expr
import           Flowbox.Luna.Data.AST.Expr                         (Expr)
import           Flowbox.Luna.Data.AST.Module                       (Module)
import qualified Flowbox.Luna.Data.AST.Lit                        as Lit
import           Flowbox.Luna.Data.AST.Lit                          (Lit)
import qualified Flowbox.Luna.Data.AST.Pat                        as Pat
import           Flowbox.Luna.Data.AST.Pat                          (Pat)
import qualified Flowbox.Luna.Data.AST.Type                       as Type
import qualified Flowbox.Luna.Data.AST.Utils                      as AST
import qualified Flowbox.Luna.Data.Attributes                     as Attributes
import           Flowbox.Luna.Data.Graph.Edge                       (Edge(Edge))
import           Flowbox.Luna.Data.Graph.Graph                      (Graph)
import qualified Flowbox.Luna.Data.Graph.Graph                    as Graph
import qualified Flowbox.Luna.Data.Graph.Node                     as Node
import           Flowbox.Luna.Data.Graph.Node                       (Node)
import           Flowbox.Luna.Data.Graph.Properties                 (Properties)
import qualified Flowbox.Luna.Passes.Pass                         as Pass
import           Flowbox.Luna.Passes.Pass                           (PassMonad)
import qualified Flowbox.Luna.Passes.Transform.Graph.Parser.State as State
import           Flowbox.Luna.Passes.Transform.Graph.Parser.State   (GPState(GPState))
import           Flowbox.System.Log.Logger                           



logger :: Logger
logger = getLogger "Flowbox.Luna.Passes.Transform.Graph.Parser.Parser"


type GPMonad m = PassMonad GPState m


run :: PassMonad s m => Graph -> Expr -> Pass.Result m Expr
run gr = (Pass.run_ (Pass.Info "GraphParser") $ State.make gr) . graph2expr


graph2expr :: GPMonad m => Expr -> Pass.Result m Expr
graph2expr expr = do 
    graph <- State.getGraph
    mapM_ parseNode $ Graph.topsortl graph
    body <- State.getBody
    return (Expr.body .~ body $ expr)


parseNode :: GPMonad m => (Node.ID, Node) -> Pass.Result m ()
parseNode (nodeID, node) = do
    case node of 
        Node.Expr expr mast properties -> case mast of
            Just ast -> State.addToBody ast
            Nothing  -> parseExprNode nodeID expr properties
        Node.Inputs  properties -> parseInputsNode  nodeID properties
        Node.Outputs properties -> parseOutputsNode nodeID properties


parseExprNode :: GPMonad m => Node.ID -> String -> Properties -> Pass.Result m ()
parseExprNode nodeID expr properties = case expr of 
    '=':pat -> parsePatNode   nodeID pat  properties
    '~':inf -> parseInfixNode nodeID expr properties
    _       -> parseAppNode   nodeID expr properties


parseInputsNode :: GPMonad m => Node.ID -> Properties -> Pass.Result m ()
parseInputsNode nodeID properties = do
    addExpr nodeID dummyExpr


parseOutputsNode :: GPMonad m => Node.ID -> Properties -> Pass.Result m ()
parseOutputsNode nodeID properties = do
    return ()


parsePatNode :: GPMonad m => Node.ID -> String -> Properties -> Pass.Result m ()
parsePatNode nodeID pat properties = do
    srcs <- State.getNodeSrcs nodeID
    case srcs of 
        [s] -> do let p = Pat.Con dummyInt pat
                      e = Expr.Assignment dummyInt p s
                  State.addToNodeMap nodeID e


parseInfixNode :: GPMonad m => Node.ID -> String -> Properties -> Pass.Result m ()
parseInfixNode nodeID inf properties = do
    srcs <- State.getNodeSrcs nodeID
    case srcs of
        [a, b] -> do let e = Expr.Infix dummyInt inf a b
                     addExpr nodeID e
        _      -> fail "parseInfixNode: Wrong Infix arguments"


parseAppNode :: GPMonad m => Node.ID -> String -> Properties -> Pass.Result m ()
parseAppNode nodeID app properties = do
    addExpr nodeID dummyExpr
    srcs <- State.getNodeSrcs nodeID
    case srcs of 
        []  -> do let e   = Expr.Con dummyInt app
                  addExpr nodeID e
        [f] -> do let e   = Expr.Accessor dummyInt app f
                  addExpr nodeID e
        f:t -> do let acc = Expr.Accessor dummyInt app f
                      e   = Expr.App      dummyInt acc t
                  addExpr nodeID e


addExpr :: GPMonad m => Node.ID -> Expr -> Pass.Result m ()
addExpr nodeID e = if dummyFolded
    then State.addToNodeMap nodeID e
    else do let p  = Pat.Var  dummyInt dummyString
                p' = Expr.Var dummyInt dummyString
                a  = Expr.Assignment dummyInt p e
            State.addToNodeMap nodeID p'
            State.addToBody a


--- REMOVE ME ------
dummyFolded = False
dummyInt = (-1)
dummyString = "dummy"
dummyList = []
dummyExpr = Expr.NOP dummyInt
dummyPat = Pat.Wildcard dummyInt
--------------------
