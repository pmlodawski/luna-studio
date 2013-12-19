 ---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------
{-# LANGUAGE ConstraintKinds           #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Flowbox.Luna.Passes.Transform.Graph.Parser.State where

import           Control.Monad.State
import qualified Data.IntMap         as IntMap
import           Data.Map            (Map)
import qualified Data.Map            as Map

import           Flowbox.Luna.Data.AliasAnalysis (AA)
import qualified Flowbox.Luna.Data.AliasAnalysis as AA
import           Flowbox.Luna.Data.AST.Expr      (Expr)
import qualified Flowbox.Luna.Data.AST.Expr      as Expr
import           Flowbox.Luna.Data.AST.Pat       (Pat)
import qualified Flowbox.Luna.Data.AST.Utils     as AST
import           Flowbox.Luna.Data.Graph.Edge    (Edge)
import           Flowbox.Luna.Data.Graph.Graph   (Graph)
import qualified Flowbox.Luna.Data.Graph.Graph   as Graph
import           Flowbox.Luna.Data.Graph.Node    (Node)
import qualified Flowbox.Luna.Data.Graph.Node    as Node
import           Flowbox.Luna.Data.Graph.Port    (OutPort)
import           Flowbox.Prelude                 hiding (mapM)
import           Flowbox.System.Log.Logger
import Flowbox.Control.Error

logger :: Logger
logger = getLogger "Flowbox.Luna.Passes.Transform.Graph.Parser.State"


type NodeMap = Map Node.ID Expr


data GPState = GPState { graph   :: Graph
                       , body    :: [Expr]
                       , nodeMap :: NodeMap
                       } deriving (Show)


type GPStateM m = MonadState GPState m


make :: Graph -> GPState
make gr = GPState gr [] Map.empty


getGraph :: GPStateM m => m Graph
getGraph = get >>= return . graph


getBody :: GPStateM m => m [Expr]
getBody = get >>= return . body


setBody :: GPStateM m => [Expr] -> m ()
setBody b = do s <- get
               put s { body = b }


getNodeMap :: GPStateM m => m NodeMap
getNodeMap = get >>= return . nodeMap


setNodeMap :: GPStateM m => NodeMap -> m ()
setNodeMap nm = do gm <- get
                   put gm { nodeMap = nm }


addToBody :: GPStateM m => Expr -> m ()
addToBody e = do b <- getBody
                 setBody $ b ++ [e]


addToNodeMap :: GPStateM m => Node.ID -> Expr -> m ()
addToNodeMap nodeID expr = getNodeMap >>= setNodeMap . Map.insert nodeID expr


nodeMapLookUp :: GPStateM m => Node.ID -> m Expr
nodeMapLookUp nodeID = do nm <- getNodeMap 
                          Map.lookup nodeID nm <?> ("nodeMapLookUp: Couldn't find nodeID=" ++ (show nodeID) ++ " in map")


getNodeSrcs :: GPStateM m => Node.ID -> m [Expr]
getNodeSrcs nodeID = do g <- getGraph
                        mapM nodeMapLookUp $ Graph.pre g nodeID
