 ---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE TemplateHaskell       #-}

module Flowbox.Luna.Passes.Transform.Graph.Parser.State where

import           Control.Monad.State
import           Data.Map            (Map)
import qualified Data.Map            as Map
import qualified Data.Maybe          as Maybe

import           Flowbox.Control.Error
import           Flowbox.Luna.Data.AST.Expr                      (Expr)
import qualified Flowbox.Luna.Data.AST.Expr                      as Expr
import qualified Flowbox.Luna.Data.Graph.Edge                    as Edge
import           Flowbox.Luna.Data.Graph.Graph                   (Graph)
import qualified Flowbox.Luna.Data.Graph.Graph                   as Graph
import           Flowbox.Luna.Data.Graph.Node                    (Node)
import qualified Flowbox.Luna.Data.Graph.Node                    as Node
import           Flowbox.Luna.Data.Graph.Port                    (OutPort)
import           Flowbox.Luna.Data.PropertyMap                   (PropertyMap)
import qualified Flowbox.Luna.Passes.Transform.AST.IDFixer.State as IDFixer
import           Flowbox.Prelude                                 hiding (mapM)
import           Flowbox.System.Log.Logger



logger :: Logger
logger = getLogger "Flowbox.Luna.Passes.Transform.Graph.Parser.State"


type NodeMap = Map (Node.ID, OutPort) Expr


data GPState = GPState { _body        :: [Expr]
                       , _output      :: Maybe Expr
                       , _nodeMap     :: NodeMap
                       , _graph       :: Graph
                       , _propertyMap :: PropertyMap
                       } deriving (Show)

makeLenses(''GPState)

type GPStateM m r = (MonadState GPState m, MonadIO m) => EitherT String m r


make :: Graph -> PropertyMap -> GPState
make = GPState [] Nothing Map.empty


getBody :: GPStateM m [Expr]
getBody = gets (view body)


setBody :: [Expr] -> GPStateM m ()
setBody b = modify (set body b)


getOutput :: GPStateM m (Maybe Expr)
getOutput = gets (view output)


setOutput :: Expr -> GPStateM m ()
setOutput o = modify (set output $ Just o)


getNodeMap :: GPStateM m NodeMap
getNodeMap = gets (view nodeMap)


setNodeMap :: NodeMap -> GPStateM m ()
setNodeMap nm =  modify (set nodeMap nm)


getGraph :: GPStateM m Graph
getGraph = gets (view graph)


getPropertyMap :: GPStateM m PropertyMap
getPropertyMap = gets (view propertyMap)


addToBody :: Expr -> GPStateM m ()
addToBody e = do b <- getBody
                 setBody $ e : b


addToNodeMap :: (Node.ID, OutPort) -> Expr -> GPStateM m ()
addToNodeMap key expr = getNodeMap >>= setNodeMap . Map.insert key expr


nodeMapLookUp :: (Node.ID, OutPort) -> GPStateM m Expr
nodeMapLookUp key = do nm <- getNodeMap
                       Map.lookup key nm <??> "GraphParser: nodeMapLookUp: Cannot find " ++ show key ++ " in nodeMap"



getNodeSrcs :: Node.ID -> GPStateM m [Expr]
getNodeSrcs nodeID = do
    g <- getGraph
    let processEdge (pNID, _, Edge.Data s d) = Just (d, (pNID, s))
        processEdge (_   , _, Edge.Monadic ) = Nothing

        connectedMap = Map.fromList
                     $ Maybe.mapMaybe processEdge
                     $ Graph.lprel g nodeID
    case Map.size connectedMap of
        0 -> return []
        _ -> do let maxPort   = fst $ Map.findMax connectedMap
                    connected = map (flip Map.lookup connectedMap) [0..maxPort]
                mapM getNodeSrc connected


getNodeSrc :: Maybe (Node.ID, OutPort) -> GPStateM m Expr
getNodeSrc Nothing  = return $ Expr.Wildcard IDFixer.unknownID
getNodeSrc (Just a) = nodeMapLookUp a


getNode :: Node.ID -> GPStateM m Node
getNode nodeID = do gr <- getGraph
                    Graph.lab gr nodeID <??> "GraphParser: getNodeOutputName: Cannot find nodeID=" ++ show nodeID ++ " in graph"


getNodeOutputName :: Node.ID -> GPStateM m String
getNodeOutputName nodeID = do node <- getNode nodeID
                              return $ node ^. Node.outputName

