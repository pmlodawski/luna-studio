 ---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------
{-# LANGUAGE ConstraintKinds  #-}
{-# LANGUAGE FlexibleContexts #-}

module Flowbox.Luna.Passes.Transform.Graph.Parser.State where

import           Control.Monad.State
import           Data.Map            (Map)
import qualified Data.Map            as Map

import           Flowbox.Control.Error
import           Flowbox.Luna.Data.AST.Expr    (Expr)
import           Flowbox.Luna.Data.Graph.Edge  (Edge (Edge))
import           Flowbox.Luna.Data.Graph.Graph (Graph)
import qualified Flowbox.Luna.Data.Graph.Graph as Graph
import           Flowbox.Luna.Data.Graph.Node  (Node)
import qualified Flowbox.Luna.Data.Graph.Node  as Node
import           Flowbox.Luna.Data.Graph.Port  (OutPort)
import           Flowbox.Luna.Data.PropertyMap (PropertyMap)
import           Flowbox.Prelude               hiding (mapM)
import           Flowbox.System.Log.Logger



logger :: Logger
logger = getLogger "Flowbox.Luna.Passes.Transform.Graph.Parser.State"


type NodeMap = Map (Node.ID, OutPort) Expr


data GPState = GPState { body        :: [Expr]
                       , nodeMap     :: NodeMap
                       , graph       :: Graph
                       , propertyMap :: PropertyMap
                       } deriving (Show)


type GPStateM m = MonadState GPState m


make :: Graph -> PropertyMap -> GPState
make = GPState [] Map.empty


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


getGraph :: GPStateM m => m Graph
getGraph = get >>= return . graph


getPropertyMap :: GPStateM m => m PropertyMap
getPropertyMap = get >>= return . propertyMap


addToBody :: GPStateM m => Expr -> m ()
addToBody e = do b <- getBody
                 setBody $ b ++ [e]


addToNodeMap :: GPStateM m => (Node.ID, OutPort) -> Expr -> m ()
addToNodeMap key expr = getNodeMap >>= setNodeMap . Map.insert key expr


nodeMapLookUp :: GPStateM m => (Node.ID, OutPort) -> m Expr
nodeMapLookUp key = do nm <- getNodeMap
                       Map.lookup key nm <?> ("nodeMapLookUp: Cannot find " ++ (show key) ++ " in nodeMap")



getNodeSrcs :: GPStateM m => Node.ID -> m [Expr]
getNodeSrcs nodeID = do g <- getGraph
                        mapM nodeMapLookUp $ map (\(pNID, _, Edge s _) -> (pNID, s))
                                           $ Graph.lprel g nodeID


getNode :: GPStateM m => Node.ID -> m Node
getNode nodeID = do gr <- getGraph
                    Graph.lab gr nodeID <?> ("getNodeOutputName: Cannot find nodeID=" ++ (show nodeID) ++ " in graph")


getNodeOutputName :: GPStateM m => Node.ID -> m String
getNodeOutputName nodeID = do node <- getNode nodeID
                              return $ node ^. Node.outputName

