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

module Luna.Pass.Transform.Graph.Parser.State where

import           Control.Monad.State
import           Data.Map            (Map)
import qualified Data.Map            as Map
import qualified Data.Maybe          as Maybe

import           Flowbox.Control.Error
import           Flowbox.Prelude                       hiding (mapM)
import           Flowbox.System.Log.Logger
import           Luna.AST.Expr                         (Expr)
import qualified Luna.AST.Expr                         as Expr
import qualified Luna.Graph.Attributes.Naming          as Attributes
import qualified Luna.Graph.Edge                       as Edge
import           Luna.Graph.Graph                      (Graph)
import qualified Luna.Graph.Graph                      as Graph
import           Luna.Graph.Node                       (Node)
import qualified Luna.Graph.Node                       as Node
import           Luna.Graph.Port                       (Port)
import qualified Luna.Graph.Port                       as Port
import           Luna.Graph.PropertyMap                (PropertyMap)
import qualified Luna.Graph.PropertyMap                as PropertyMap
import           Luna.Info                             (apiVersion)
import           Luna.Pass.Pass                        (Pass)
import qualified Luna.Pass.Transform.AST.IDFixer.State as IDFixer



logger :: Logger
logger = getLogger $(moduleName)


type NodeMap = Map (Node.ID, Port) Expr


data GPState = GPState { _body        :: [Expr]
                       , _output      :: Maybe Expr
                       , _nodeMap     :: NodeMap
                       , _graph       :: Graph
                       , _propertyMap :: PropertyMap
                       } deriving (Show)

makeLenses(''GPState)


type GPPass result = Pass GPState result


make :: Graph -> PropertyMap -> GPState
make = GPState [] Nothing Map.empty


getBody :: GPPass [Expr]
getBody = gets (view body)


setBody :: [Expr] -> GPPass ()
setBody b = modify (set body b)


getOutput :: GPPass (Maybe Expr)
getOutput = gets (view output)


setOutput :: Expr -> GPPass ()
setOutput o = modify (set output $ Just o)


getNodeMap :: GPPass NodeMap
getNodeMap = gets (view nodeMap)


setNodeMap :: NodeMap -> GPPass ()
setNodeMap nm = modify (set nodeMap nm)


getGraph :: GPPass Graph
getGraph = gets (view graph)


getPropertyMap :: GPPass PropertyMap
getPropertyMap = gets (view propertyMap)


setPropertyMap :: PropertyMap -> GPPass ()
setPropertyMap pm =  modify (set propertyMap pm)


addToBody :: Expr -> GPPass ()
addToBody e = do b <- getBody
                 setBody $ e : b


addToNodeMap :: (Node.ID, Port) -> Expr -> GPPass ()
addToNodeMap key expr = getNodeMap >>= setNodeMap . Map.insert key expr


nodeMapLookup :: (Node.ID, Port) -> GPPass Expr
nodeMapLookup key = do
    nm <- getNodeMap
    Map.lookup key nm <??> "GraphParser: nodeMapLookup: Cannot find " ++ show key ++ " in nodeMap"


getNodeSrcs :: Node.ID -> GPPass [Expr]
getNodeSrcs nodeID = do
    g <- getGraph
    let processEdge (pNID, _, Edge.Data s  Port.All   ) = Just (0, (pNID, s))
        processEdge (pNID, _, Edge.Data s (Port.Num d)) = Just (d, (pNID, s))
        processEdge (_   , _, Edge.Monadic            ) = Nothing

        connectedMap = Map.fromList
                     $ Maybe.mapMaybe processEdge
                     $ Graph.lprel g nodeID
    case Map.size connectedMap of
        0 -> return []
        _ -> do let maxPort   = fst $ Map.findMax connectedMap
                    connected = map (flip Map.lookup connectedMap) [0..maxPort]
                mapM getNodeSrc connected


inboundPorts :: Node.ID -> GPPass [Port]
inboundPorts nodeID = do
    g <- getGraph
    let processEdge (_, Edge.Data _ d) = Just d
        processEdge (_, Edge.Monadic ) = Nothing
    return $ Maybe.mapMaybe processEdge
           $ Graph.lpre g nodeID


getNodeSrc :: Maybe (Node.ID, Port) -> GPPass Expr
getNodeSrc Nothing  = return $ Expr.Wildcard IDFixer.unknownID
getNodeSrc (Just a) = nodeMapLookup a


getNode :: Node.ID -> GPPass Node
getNode nodeID = do gr <- getGraph
                    Graph.lab gr nodeID <??> "GraphParser: getNodeOutputName: Cannot find nodeID=" ++ show nodeID ++ " in graph"


getNodeOutputName :: Node.ID -> GPPass String
getNodeOutputName nodeID = view Node.outputName <$> getNode nodeID


getProperty :: Node.ID -> String -> GPPass (Maybe String)
getProperty nodeID propertyName =
    PropertyMap.get nodeID (show apiVersion) propertyName <$> getPropertyMap


hasFlag :: Node.ID -> String -> GPPass Bool
hasFlag nodeID flag =
    (== Just Attributes.true) <$> getProperty nodeID flag


setProperty :: Node.ID -> String -> String -> GPPass ()
setProperty nodeID key value =
    getPropertyMap >>=
    setPropertyMap . PropertyMap.set nodeID (show apiVersion) key value


setPosition :: Node.ID -> (Float, Float) -> GPPass ()
setPosition nodeID position =
    setProperty nodeID Attributes.nodePosition $ show position


setGraphFolded :: Node.ID -> GPPass ()
setGraphFolded nodeID =
    setProperty nodeID Attributes.graphFolded Attributes.true
    --return ()



doesLastStatementReturn :: GPPass Bool
doesLastStatementReturn = do
    body' <- getBody
    return $ case body' of
        []                       -> False
        (Expr.Assignment {} : _) -> False
        _                        -> True
