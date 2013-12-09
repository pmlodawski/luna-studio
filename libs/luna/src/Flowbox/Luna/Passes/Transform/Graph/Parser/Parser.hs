---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------
{-# LANGUAGE FlexibleContexts, NoMonomorphismRestriction, ConstraintKinds #-}

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


run :: PassMonad s m => Graph -> Pass.Result m Expr
run gr = (Pass.run_ (Pass.Info "GraphParser") $ State.make gr) graph2expr


graph2expr :: GPMonad m => Pass.Result m Expr
graph2expr = do 
    graph <- State.getGraph
    let nodes = Graph.labNodes graph
    mapM_ node2expr nodes

    GPState _ body <- get
    
    return $ Expr.Function dummyInt dummyList dummyString dummyList dummyType body


node2expr :: GPMonad m => (Node.ID, Node) -> Pass.Result m ()
node2expr (nodeID, node) = do
    graph <- State.getGraph
    case node of 
        Node.Expr expr mast properties -> case mast of
            Just ast -> State.addToBody ast
            Nothing  -> exprNode2expr expr properties
        Node.Inputs  properties -> inputsNode2expr  properties
        Node.Outputs properties -> outputsNode2expr properties


exprNode2expr :: GPMonad m => String -> Properties -> Pass.Result m ()
exprNode2expr expr properties = do
    return ()


inputsNode2expr :: GPMonad m => Properties -> Pass.Result m ()
inputsNode2expr properties = do
    return ()


outputsNode2expr :: GPMonad m => Properties -> Pass.Result m ()
outputsNode2expr properties = do
    return ()


--- REMOVE ME ------
dummyInt = (-1)
dummyString = "dummy"
dummyList = []
dummyType = Type.Unknown dummyInt
--------------------
