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
import           Luna.Pass.Pass                        (PassMonad)
import qualified Luna.Pass.Transform.AST.IDFixer.State as IDFixer
import qualified Luna.Syntax.Enum                      as Enum
import           Luna.Syntax.Expr                      (LExpr)
import qualified Luna.Syntax.Expr                      as Expr
import qualified Luna.Syntax.Graph.Edge                as Edge
import           Luna.Syntax.Graph.Flags               (Flags)
import qualified Luna.Syntax.Graph.Flags               as Flags
import           Luna.Syntax.Graph.Graph               (Graph)
import qualified Luna.Syntax.Graph.Graph               as Graph
import           Luna.Syntax.Graph.Node                (Node)
import qualified Luna.Syntax.Graph.Node                as Node
import           Luna.Syntax.Graph.Node.Position       (Position)
import           Luna.Syntax.Graph.Port                (Port)
import qualified Luna.Syntax.Graph.Port                as Port
import           Luna.Syntax.Graph.PropertyMap         (PropertyMap)
import qualified Luna.Syntax.Graph.PropertyMap         as PropertyMap
import           Luna.Syntax.Label                     (Label (Label))
import           Luna.Syntax.Name                      (VNameP)



logger :: Logger
logger = getLogger $(moduleName)


type NodeMap a v = Map (Node.ID, Port) (LExpr a v)


data GPState a v = GPState { _body        :: [LExpr a v]
                           , _output      :: Maybe (LExpr a v)
                           , _nodeMap     :: NodeMap a v
                           , _graph       :: Graph a v
                           , _propertyMap :: PropertyMap a v
                           } deriving (Show)

makeLenses ''GPState


type GPPass a v m result = (Monad m, Enum.Enumerated a)
                         => PassMonad (GPState a v) m result


make :: Graph a v -> PropertyMap a v -> GPState a v
make = GPState [] Nothing Map.empty


getBody :: GPPass a v m [LExpr a v]
getBody = gets (view body)


setBody :: [LExpr a v] -> GPPass a v m ()
setBody b = modify (set body b)


getOutput :: GPPass a v m (Maybe (LExpr a v))
getOutput = gets (view output)


setOutput :: LExpr a v -> GPPass a v m ()
setOutput o = modify (set output $ Just o)


getNodeMap :: GPPass a v m (NodeMap a v)
getNodeMap = gets (view nodeMap)


setNodeMap :: NodeMap a v -> GPPass a v m ()
setNodeMap nm = modify (set nodeMap nm)


getGraph :: GPPass a v m (Graph a v)
getGraph = gets (view graph)


getPropertyMap :: GPPass a v m (PropertyMap a v)
getPropertyMap = gets (view propertyMap)


setPropertyMap :: PropertyMap a v -> GPPass a v m ()
setPropertyMap pm =  modify (set propertyMap pm)


addToBody :: LExpr a v -> GPPass a v m ()
addToBody e = do b <- getBody
                 setBody $ e : b


addToNodeMap :: (Node.ID, Port) -> LExpr a v -> GPPass a v m ()
addToNodeMap key expr = getNodeMap >>= setNodeMap . Map.insert key expr


nodeMapLookup :: (Node.ID, Port) -> GPPass a v m (LExpr a v)
nodeMapLookup key = do
    nm <- getNodeMap
    lift $ Map.lookup key nm <??> "GraphParser: nodeMapLookup: Cannot find " ++ show key ++ " in nodeMap"


getNodeSrcs :: Node.ID -> GPPass a v m [LExpr a v]
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


inboundPorts :: Node.ID -> GPPass a v m [Port]
inboundPorts nodeID = do
    g <- getGraph
    let processEdge (_, Edge.Data _ d) = Just d
        processEdge (_, Edge.Monadic ) = Nothing
    return $ Maybe.mapMaybe processEdge
           $ Graph.lpre g nodeID


getNodeSrc :: Maybe (Node.ID, Port) -> GPPass a v m (LExpr a v)
getNodeSrc Nothing  = return $ Label (Enum.tag IDFixer.unknownID) Expr.Wildcard
getNodeSrc (Just a) = nodeMapLookup a


getNode :: Node.ID -> GPPass a v m (Node a v)
getNode nodeID = do
    gr <- getGraph
    lift $ Graph.lab gr nodeID <??> "GraphParser: getNodeOutputName: Cannot find nodeID=" ++ show nodeID ++ " in graph"


getNodeOutputName :: Node.ID -> GPPass a v m VNameP
getNodeOutputName nodeID = do
    outputName <- preview Node.outputName <$> getNode nodeID
    lift $ outputName <??> "GraphParser: getNodeOutputName"


getFlags :: Node.ID -> GPPass a v m Flags
getFlags nodeID = PropertyMap.getFlags nodeID <$> getPropertyMap


modifyFlags :: (Flags -> Flags) -> Node.ID -> GPPass a v m ()
modifyFlags fun nodeID =
    getPropertyMap >>= setPropertyMap . PropertyMap.modifyFlags fun nodeID


setPosition :: Node.ID -> Position -> GPPass a v m ()
setPosition nodeID position =
    modifyFlags (Flags.nodePosition .~ Just position) nodeID


setGraphFolded :: Node.ID -> GPPass a v m ()
setGraphFolded = modifyFlags (Flags.graphFoldInfo .~ Just Flags.Folded)


setGraphFoldTop :: Node.ID -> Node.ID -> GPPass a v m ()
setGraphFoldTop nodeID topID =
    modifyFlags (Flags.graphFoldInfo .~ Just (Flags.FoldTop topID)) nodeID


doesLastStatementReturn :: GPPass a v m Bool
doesLastStatementReturn = do
    body' <- getBody
    return $ case body' of
        []                                 -> False
        (Label _ (Expr.Assignment {}) : _) -> False --TODO[PM] : check it
        _                                  -> True
