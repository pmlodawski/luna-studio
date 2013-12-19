---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}

module Flowbox.Luna.Passes.Transform.Graph.Parser.Parser where

import           Control.Monad.State                                 

import           Flowbox.Prelude                                  hiding (error, mapM, mapM_)
import qualified Flowbox.Luna.Data.AST.Expr                       as Expr
import           Flowbox.Luna.Data.AST.Expr                         (Expr)
import qualified Flowbox.Luna.Data.AST.Pat                        as Pat
import           Flowbox.Luna.Data.Graph.Graph                      (Graph)
import qualified Flowbox.Luna.Data.Graph.Graph                    as Graph
import qualified Flowbox.Luna.Data.Graph.Node                     as Node
import qualified Flowbox.Luna.Data.Graph.Port                     as Port
import           Flowbox.Luna.Data.Graph.Node                       (Node)
import           Flowbox.Luna.Data.Graph.Properties                 (Properties)
import qualified Flowbox.Luna.Passes.Pass                         as Pass
import           Flowbox.Luna.Passes.Pass                           (PassMonad)
import qualified Flowbox.Luna.Passes.Transform.Graph.Parser.State as State
import           Flowbox.Luna.Passes.Transform.Graph.Parser.State   (GPState)
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
    '~':inf -> parseInfixNode nodeID expr properties
    _       -> parseAppNode   nodeID expr properties


parseInputsNode :: GPMonad m => Node.ID -> [Expr] -> Properties -> Pass.Result m ()
parseInputsNode nodeID inputs properties = do
    mapM_ (parseArg nodeID properties) $ zip [0..] inputs


parseArg :: State.GPStateM m => Node.ID -> Properties -> (Int, Expr) -> m ()
parseArg nodeID properties (num, input) = case input of
    Expr.Arg i (Pat.Var j name) value -> State.addToNodeMap (nodeID, Port.Num num) $ Expr.Var dummyInt name
    _ -> fail "parseArg: Wrong Arg type"

parseOutputsNode :: GPMonad m => Node.ID -> Properties -> Pass.Result m ()
parseOutputsNode nodeID properties = do
    return ()


parsePatNode :: GPMonad m => Node.ID -> String -> Properties -> Pass.Result m ()
parsePatNode nodeID pat properties = do
    srcs <- State.getNodeSrcs nodeID
    case srcs of 
        [s] -> do let p = dummyPat pat
                      e = Expr.Assignment dummyInt p s
                  State.addToNodeMap (nodeID, Port.All) e
        _      -> fail "parsePatNode: Wrong Pat arguments"


parseInfixNode :: GPMonad m => Node.ID -> String -> Properties -> Pass.Result m ()
parseInfixNode nodeID inf properties = do
    srcs <- State.getNodeSrcs nodeID
    case srcs of
        [a, b] -> do let e = Expr.Infix dummyInt inf a b
                     addExpr nodeID e
        _      -> fail "parseInfixNode: Wrong Infix arguments"


parseAppNode :: GPMonad m => Node.ID -> String -> Properties -> Pass.Result m ()
parseAppNode nodeID app properties = do
    srcs <- State.getNodeSrcs nodeID
    case srcs of 
        []  -> do let e   = dummyExpr app
                  addExpr nodeID e
        [f] -> do let e   = Expr.Accessor dummyInt app f
                  addExpr nodeID e
        f:t -> do let acc = Expr.Accessor dummyInt app f
                      e   = Expr.App      dummyInt acc t
                  addExpr nodeID e


addExpr :: GPMonad m => Node.ID -> Expr -> Pass.Result m ()
addExpr nodeID e = if dummyFolded
    then State.addToNodeMap (nodeID, Port.All) e
    else do outputName <- State.getNodeOutputName nodeID
            let p  = Pat.Var  dummyInt outputName
                p' = Expr.Var dummyInt outputName
                a  = Expr.Assignment dummyInt p e
            State.addToNodeMap (nodeID, Port.All) p'
            State.addToBody a


--- REMOVE ME ------
dummyFolded = False
dummyString = "dummy"
dummyInt = (-1)
dummyExpr name = Expr.Con dummyInt name
dummyPat name = Pat.Con dummyInt name
--------------------
