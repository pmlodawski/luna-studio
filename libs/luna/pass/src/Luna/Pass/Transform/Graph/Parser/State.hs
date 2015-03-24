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

import           Control.Monad.State hiding (mapM)
import           Data.Map            (Map)
import qualified Data.Map            as Map
import qualified Data.Maybe          as Maybe

import           Flowbox.Control.Error
import           Flowbox.Prelude
import           Flowbox.System.Log.Logger
import qualified Luna.Syntax.Enum          as Enum
import qualified Luna.Syntax.Expr          as Expr
import qualified Luna.Syntax.Graph.Edge    as Edge
import           Luna.Syntax.Graph.Graph   (Graph)
import qualified Luna.Syntax.Graph.Graph   as Graph
import qualified Luna.Syntax.Graph.Node    as Node
import           Luna.Syntax.Graph.Port    (DstPortP (DstPort), SrcPort)
import qualified Luna.Syntax.Graph.Port    as Port
import           Luna.Syntax.Graph.Tag     (TExpr, Tag)
import           Luna.Syntax.Label         (Label (Label))



dummyTag = Enum.tag (-999)


logger :: Logger
logger = getLogger $moduleName


type Error = String

type VarMap v = Map (Node.ID, SrcPort) (Expr.Variable v)


data GPState v = GPState { _body   :: [TExpr v]
                         , _output :: Maybe (TExpr v)
                         , _varMap :: VarMap v
                         , _graph  :: Graph Tag v
                         } deriving (Show)

makeLenses ''GPState


type GPPass v m result = Monad m => StateT (GPState v) (EitherT Error m) result


mk :: Graph Tag v -> GPState v
mk = GPState def def def


----- body ----------------------------------------------------------------
getBody :: GPPass v m [TExpr v]
getBody = gets $ view body

setBody :: [TExpr v] -> GPPass v m ()
setBody = modify . set body

addToBody :: TExpr v -> GPPass v m ()
addToBody e = setBody . (e :) =<< getBody

----- output --------------------------------------------------------------
getOutput :: GPPass v m (Maybe (TExpr v))
getOutput = gets $ view output

setOutput :: TExpr v -> GPPass v m ()
setOutput = modify . set output . Just

----- varMap -------------------------------------------------------------
getVarMap :: GPPass v m (VarMap v)
getVarMap = gets $ view varMap

setVarMap :: VarMap v -> GPPass v m ()
setVarMap = modify . set varMap

----- graph ---------------------------------------------------------------
getGraph :: GPPass v m (Graph Tag v)
getGraph = gets $ view graph


--getPropertyMap :: GPPass a v m (PropertyMap a v)
--getPropertyMap = gets (view propertyMap)


--setPropertyMap :: PropertyMap a v -> GPPass a v m ()
--setPropertyMap pm =  modify (set propertyMap pm)




--addToNodeMap :: (Node.ID, Port) -> LExpr a v -> GPPass a v m ()
--addToNodeMap key expr = getNodeMap >>= setNodeMap . Map.insert key expr


varMapLookup :: (Node.ID, SrcPort) -> GPPass v m (Expr.Variable v)
varMapLookup key = do
    nm <- getVarMap
    lift $ Map.lookup key nm <??> "GraphParser: varMapLookup: Cannot find " ++ show key ++ " in nodeMap"


getNodeSrcs :: Node.ID -> GPPass v m [Expr.AppArg (TExpr v)]
getNodeSrcs nodeID = do
    g <- getGraph
    let connectedMap = Map.fromList
                     $ Maybe.mapMaybe processEdge
                     $ Graph.lprel g nodeID
    case Map.size connectedMap of
        0 -> return []
        _ -> do let maxPort   = fst $ Map.findMax connectedMap
                    connected = map (flip Map.lookup connectedMap) [0..maxPort]
                mapM getNodeSrc connected
    where
        processEdge (pNID, _, Edge.Data s (DstPort (Port.All  ))) = Just (0, (pNID, s))
        processEdge (pNID, _, Edge.Data s (DstPort (Port.Num d))) = Just (d, (pNID, s))
        processEdge (_   , _, Edge.Monadic                      ) = Nothing

        getNodeSrc :: Maybe (Node.ID, SrcPort) -> GPPass v m (Expr.AppArg (TExpr v))
        getNodeSrc Nothing  = return $ Expr.unnamed $ Label dummyTag Expr.Wildcard
        getNodeSrc (Just a) = Expr.unnamed . Label dummyTag . Expr.Var <$> varMapLookup a


--inboundPorts :: Node.ID -> GPPass a v m [Port]
--inboundPorts nodeID = do
--    g <- getGraph
--    let processEdge (_, Edge.Data _ d) = Just d
--        processEdge (_, Edge.Monadic ) = Nothing
--    return $ Maybe.mapMaybe processEdge
--           $ Graph.lpre g nodeID


--getNode :: Node.ID -> GPPass a v m (Node a v)
--getNode nodeID = do
--    gr <- getGraph
--    lift $ Graph.lab gr nodeID <??> "GraphParser: getNodeOutputName: Cannot find nodeID=" ++ show nodeID ++ " in graph"


--getNodeOutputName :: Node.ID -> GPPass a v m VNameP
--getNodeOutputName nodeID = do
--    outputName <- preview Node.outputName <$> getNode nodeID
--    lift $ outputName <??> "GraphParser: getNodeOutputName"


--getFlags :: Node.ID -> GPPass a v m Flags
--getFlags nodeID = PropertyMap.getFlags nodeID <$> getPropertyMap


--modifyFlags :: (Flags -> Flags) -> Node.ID -> GPPass a v m ()
--modifyFlags fun nodeID =
--    getPropertyMap >>= setPropertyMap . PropertyMap.modifyFlags fun nodeID


--setPosition :: Node.ID -> Position -> GPPass a v m ()
--setPosition nodeID position =
--    modifyFlags (Flags.nodePosition .~ Just position) nodeID


--setGraphFolded :: Node.ID -> GPPass a v m ()
--setGraphFolded = modifyFlags (Flags.graphFoldInfo .~ Just Flags.Folded)


--setGraphFoldTop :: Node.ID -> Node.ID -> GPPass a v m ()
--setGraphFoldTop nodeID topID =
--    modifyFlags (Flags.graphFoldInfo .~ Just (Flags.FoldTop topID)) nodeID


--doesLastStatementReturn :: GPPass a v m Bool
--doesLastStatementReturn = do
--    body' <- getBody
--    return $ case body' of
--        []                                 -> False
--        (Label _ (Expr.Assignment {}) : _) -> False --TODO[PM] : check it
--        _                                  -> True
