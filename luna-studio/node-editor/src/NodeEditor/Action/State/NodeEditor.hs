{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE LambdaCase             #-}
{-# LANGUAGE TypeFamilies           #-}
module NodeEditor.Action.State.NodeEditor where

import           Common.Prelude                              hiding (get)

import qualified Control.Monad.State                         as M
import qualified Data.HashMap.Strict                         as HashMap
import qualified Data.Map.Lazy                               as Map
import qualified Data.Set                                    as Set
import qualified JS.Visualizers                              as JS
import qualified LunaStudio.Data.NodeSearcher                as NS
import qualified LunaStudio.Data.PortRef                     as PortRef
import qualified NodeEditor.Action.Batch                     as Batch
import qualified NodeEditor.Action.State.Internal.NodeEditor as Internal
import qualified NodeEditor.React.Model.Layout               as Scene
import qualified NodeEditor.React.Model.Node.ExpressionNode  as ExpressionNode
import qualified NodeEditor.React.Model.NodeEditor           as NE
import qualified NodeEditor.React.Model.Port                 as Port
import qualified NodeEditor.React.Model.Searcher             as Searcher
import qualified NodeEditor.React.Model.Visualization        as Visualization
import qualified NodeEditor.State.Global                     as Global

import           Common.Action.Command                       (Command)
import           Control.Arrow                               ((&&&))
import           Data.Map.Lazy                               (Map)
import           Data.Monoid                                 (First (First), getFirst)
import           LunaStudio.Data.CameraTransformation        (CameraTransformation)
import           LunaStudio.Data.GraphLocation               (breadcrumb)
import           LunaStudio.Data.MonadPath                   (MonadPath)
import           LunaStudio.Data.NodeMeta                    (NodeMeta)
import           LunaStudio.Data.NodeSearcher                (ImportName, ModuleHints)
import           LunaStudio.Data.Port                        (_WithDefault)
import           LunaStudio.Data.PortDefault                 (PortDefault)
import           LunaStudio.Data.PortRef                     (AnyPortRef (..), InPortRef (..), OutPortRef (..))
import           LunaStudio.Data.Position                    (Position)
import           LunaStudio.Data.TypeRep                     (TypeRep (TStar), toConstructorRep)
import           LunaStudio.Data.Visualizer                  (applyType, fromJSInternalVisualizersMap, fromJSVisualizersMap)
import           NodeEditor.Action.State.App                 (get, getWorkspace, modify, modifyApp)
import           NodeEditor.Action.UUID                      (getUUID)
import           NodeEditor.Batch.Workspace                  (currentLocation)
import           NodeEditor.Data.Graph                       (Graph (Graph))
import           NodeEditor.React.Model.App                  (nodeEditor)
import           NodeEditor.React.Model.Breadcrumbs          (isTopLevel)
import           NodeEditor.React.Model.Connection           (Connection, ConnectionId, ConnectionsMap, HalfConnection, PosConnection,
                                                              connectionId, containsNode, containsPortRef, dstNodeLoc, srcNodeLoc,
                                                              toConnectionsMap)
import           NodeEditor.React.Model.Layout               (Layout, Scene)
import           NodeEditor.React.Model.Node                 (InputNode, Node (Expression, Input, Output), NodeLoc, OutputNode, inPortAt,
                                                              inPortsList, nodeLoc, outPortAt, outPortsList, toNodesMap)
import           NodeEditor.React.Model.Node.ExpressionNode  (_ShortValue,_Error, ExpressionNode, isSelected, Value (AwaitingData, AwaitingTypecheck, Error, ShortValue))
import           NodeEditor.React.Model.NodeEditor           (GraphStatus, NodeEditor, VisualizationBackup,
                                                              VisualizersPaths (VisualizersPaths))
import           NodeEditor.React.Model.Port                 (InPort, OutPort, state)
import           NodeEditor.React.Model.Searcher             (Searcher)
import           NodeEditor.State.Global                     (State, internalVisualizers, nodeSearcherData, preferedVisualizers,
                                                              visualizers)


getNodeEditor :: Command State NodeEditor
getNodeEditor = get nodeEditor

modifyNodeEditor :: M.State NodeEditor r -> Command State r
modifyNodeEditor = modify nodeEditor

inTopLevelBreadcrumb :: Command State Bool
inTopLevelBreadcrumb = inTopLevelBreadcrumb' <$> getWorkspace where
    inTopLevelBreadcrumb' Nothing = False
    inTopLevelBreadcrumb' (Just w)
        = isTopLevel $ w ^. currentLocation . breadcrumb

isGraphLoaded :: Command State Bool
isGraphLoaded = view NE.isGraphLoaded <$> getNodeEditor

whenGraphLoaded :: Command State () -> Command State ()
whenGraphLoaded = whenM isGraphLoaded

setGraphStatus :: GraphStatus -> Command State ()
setGraphStatus graphStatus = modifyNodeEditor $ NE.graphStatus .= graphStatus

resetGraph :: Command State ()
resetGraph = modifyNodeEditor $ M.put def

resetApp :: Command State ()
resetApp = modifyApp $ M.put def

separateSubgraph :: [NodeLoc] -> Command State Graph
separateSubgraph nodeLocs = do
    let idSet = Set.fromList nodeLocs
        inSet = flip Set.member idSet
        mkMap = HashMap.fromList . map (view nodeLoc &&& id)
    nodes'
        <- HashMap.filterWithKey (inSet .: const) . mkMap <$> getExpressionNodes
    conns' <- HashMap.filter (inSet . view dstNodeLoc) <$> getConnectionsMap
    return $ Graph (HashMap.map convert nodes') (HashMap.map convert conns')

addNode :: Node -> Command State ()
addNode (Expression node) = addExpressionNode node
addNode (Input      node) = addInputNode node
addNode (Output     node) = addOutputNode node

addExpressionNode :: ExpressionNode -> Command State ()
addExpressionNode node = Internal.addNodeRec
    NE.expressionNodes
    ExpressionNode.expressionNodes
    (node ^. nodeLoc)
    node

addInputNode :: InputNode -> Command State ()
addInputNode node = Internal.setNodeRec
    NE.inputNode
    ExpressionNode.inputNode
    (node ^. nodeLoc)
    node

addOutputNode :: OutputNode -> Command State ()
addOutputNode node = Internal.setNodeRec
    NE.outputNode
    ExpressionNode.outputNode
    (node ^. nodeLoc)
    node

findPredecessorPosition :: ExpressionNode -> Command State Position
findPredecessorPosition n
    = ExpressionNode.findPredecessorPosition n <$> getExpressionNodes

findSuccessorPosition :: ExpressionNode -> Command State Position
findSuccessorPosition n
    = ExpressionNode.findSuccessorPosition n <$> getExpressionNodes

getNode :: NodeLoc -> Command State (Maybe Node)
getNode nl = NE.getNode nl <$> getNodeEditor

getAllNodes :: Command State [Node]
getAllNodes = do
    inputNodes      <- getInputNodes
    outputNodes     <- getOutputNodes
    expressionNodes <- getExpressionNodes
    return
        $  map Input      inputNodes
        <> map Output     outputNodes
        <> map Expression expressionNodes

getInputNode :: NodeLoc -> Command State (Maybe InputNode)
getInputNode nl = NE.getInputNode nl <$> getNodeEditor

getOutputNode :: NodeLoc -> Command State (Maybe OutputNode)
getOutputNode nl = NE.getOutputNode nl <$> getNodeEditor

getInputNodes :: Command State [InputNode]
getInputNodes = view NE.inputNodesRecursive <$> getNodeEditor

getOutputNodes :: Command State [OutputNode]
getOutputNodes = view NE.outputNodesRecursive <$> getNodeEditor

getExpressionNode :: NodeLoc -> Command State (Maybe ExpressionNode)
getExpressionNode nl = NE.getExpressionNode nl <$> getNodeEditor

getExpressionNodes :: Command State [ExpressionNode]
getExpressionNodes = view NE.expressionNodesRecursive <$> getNodeEditor

modifyExpressionNode :: Monoid r
    => NodeLoc -> M.State ExpressionNode r -> Command State r
modifyExpressionNode
  = Internal.modifyNodeRec NE.expressionNodes ExpressionNode.expressionNodes

modifyExpressionNodes_ :: M.State ExpressionNode () -> Command State ()
modifyExpressionNodes_ = void . modifyExpressionNodes

modifyExpressionNodes :: M.State ExpressionNode r -> Command State [r]
modifyExpressionNodes modifier = do
    nodeLocs <- view ExpressionNode.nodeLoc `fmap2` getExpressionNodes --FIXME it can be done faster
    catMaybes . map getFirst <$> forM
        nodeLocs
        (flip modifyExpressionNode $ (fmap (First . Just) modifier))

modifyInputNode :: Monoid r
    => NodeLoc -> M.State InputNode r -> Command State r
modifyInputNode
    = Internal.modifySidebarRec NE.inputNode ExpressionNode.inputNode

modifyOutputNode :: Monoid r
    => NodeLoc -> M.State OutputNode r -> Command State r
modifyOutputNode
    = Internal.modifySidebarRec NE.outputNode ExpressionNode.outputNode

removeNode :: NodeLoc -> Command State ()
removeNode
    = Internal.removeNodeRec NE.expressionNodes ExpressionNode.expressionNodes

getSelectedNodes :: Command State [ExpressionNode]
getSelectedNodes = filter (view isSelected) <$> getExpressionNodes

updateInputNode :: Maybe InputNode -> Command State ()
updateInputNode update = modifyNodeEditor $ NE.inputNode .= update

updateOutputNode :: Maybe OutputNode -> Command State ()
updateOutputNode update = modifyNodeEditor $ NE.outputNode .= update

updateExpressionNodes :: [ExpressionNode] -> Command State ()
updateExpressionNodes update
    = modifyNodeEditor $ NE.expressionNodes .= toNodesMap update

addConnection :: Connection -> Command State ()
addConnection conn = modifyNodeEditor $ NE.connections . at connId ?= conn where
    connId = conn ^. connectionId

getConnection :: ConnectionId -> Command State (Maybe Connection)
getConnection connId = HashMap.lookup connId <$> getConnectionsMap

getConnections :: Command State [Connection]
getConnections = HashMap.elems <$> getConnectionsMap

getPosConnection :: ConnectionId -> Command State (Maybe PosConnection)
getPosConnection connId = do
    mayConnection <- getConnection connId
    ne <- getNodeEditor
    return $ join $ NE.toPosConnection ne <$> mayConnection

getPosConnections :: Command State [PosConnection]
getPosConnections = view NE.posConnections <$> getNodeEditor

getConnectionsMap :: Command State ConnectionsMap
getConnectionsMap = view NE.connections <$> getNodeEditor

getConnectionsBetweenNodes :: NodeLoc -> NodeLoc -> Command State [Connection]
getConnectionsBetweenNodes nl1 nl2 = filter
    (\conn -> containsNode nl1 conn && containsNode nl2 conn)
    <$> getConnections

getConnectionsContainingNode :: NodeLoc -> Command State [Connection]
getConnectionsContainingNode nl = filter (containsNode nl) <$> getConnections

getConnectionsContainingNodes :: [NodeLoc] -> Command State [Connection]
getConnectionsContainingNodes nodeLocs
    = filter containsNode' <$> getConnections where
        nodeLocsSet = Set.fromList nodeLocs
        containsNode' conn
            = Set.member (conn ^. srcNodeLoc) nodeLocsSet
            || Set.member (conn ^. dstNodeLoc) nodeLocsSet

getConnectionsContainingPortRef :: AnyPortRef -> Command State [Connection]
getConnectionsContainingPortRef portRef
    = filter (containsPortRef portRef) <$> getConnections

getConnectionsFromNode :: NodeLoc -> Command State [Connection]
getConnectionsFromNode nl
    = filter (\conn -> conn ^. srcNodeLoc == nl) <$> getConnections

getConnectionsToNode :: NodeLoc -> Command State [Connection]
getConnectionsToNode nl
    = filter (\conn -> conn ^. dstNodeLoc == nl) <$> getConnections

modifyConnection :: Monoid r
    => ConnectionId -> M.State Connection r -> Command State r
modifyConnection connId
    = modify (nodeEditor . NE.connections . at connId) . zoom traverse

removeConnection :: ConnectionId -> Command State ()
removeConnection connId
    = modifyNodeEditor $ NE.connections . at connId .= Nothing

updateConnections :: [Connection] -> Command State ()
updateConnections update
    = modifyNodeEditor $ NE.connections .= toConnectionsMap update


getMonads :: Command State [MonadPath]
getMonads = view NE.monads <$> getNodeEditor

updateMonads :: [MonadPath] -> Command State ()
updateMonads update = modifyNodeEditor $ NE.monads .= update

getNodeMeta :: NodeLoc -> Command State (Maybe NodeMeta)
getNodeMeta = fmap2 (view ExpressionNode.nodeMeta) . getExpressionNode

modifyHalfConnections :: M.State [HalfConnection] r -> Command State r
modifyHalfConnections = modify (nodeEditor . NE.halfConnections)

getSearcher :: Command State (Maybe Searcher)
getSearcher = view NE.searcher <$> getNodeEditor

modifySearcher :: Monoid r => M.State Searcher r -> Command State r
modifySearcher = modify (nodeEditor . NE.searcher) . zoom traverse

getLayout :: Command State Layout
getLayout = view NE.layout <$> getNodeEditor

getScene :: Command State (Maybe Scene)
getScene = view Scene.scene <$> getLayout

getScreenTransform :: Command State CameraTransformation
getScreenTransform = view Scene.screenTransform <$> getLayout

setScreenTransform :: CameraTransformation -> Command State ()
setScreenTransform camera
    = modifyNodeEditor $ NE.layout . Scene.screenTransform .= camera

getNodeSearcherData :: Command State (Map ImportName ModuleHints)
getNodeSearcherData = getAvailableImports <$> use nodeSearcherData where
    getAvailableImports nsd = Map.filterWithKey
        (\k _ -> Set.member k $ nsd ^. NS.currentImports)
        $ nsd ^. NS.imports

class NodeEditorElementId a where
    inGraph :: a -> Command State Bool
instance NodeEditorElementId NodeLoc where
    inGraph = fmap isJust . getNode
instance NodeEditorElementId ConnectionId where
    inGraph = fmap isJust . getConnection

getPort :: NE.GetPort a b => a -> Command State (Maybe b)
getPort portRef = NE.getPort portRef <$> getNodeEditor

modifyInPort :: Monoid r => InPortRef -> M.State InPort r -> Command State r
modifyInPort portRef action = modifyExpressionNode (portRef ^. PortRef.nodeLoc)
    $ zoom (inPortAt $ portRef ^. PortRef.dstPortId) action

modifyInPortsForNode :: Monoid r
    => NodeLoc -> M.State InPort r -> Command State ()
modifyInPortsForNode nl action = withJustM (getExpressionNode nl)
     $ \n -> mapM_
        (flip modifyInPort action)
        (InPortRef nl . view Port.portId <$> inPortsList n)

modifyOutPort :: Monoid r => OutPortRef -> M.State OutPort r -> Command State r
modifyOutPort portRef action = modifyExpressionNode (portRef ^. PortRef.nodeLoc)
    $ zoom (outPortAt $ portRef ^. PortRef.srcPortId) action

modifyOutPortsForNode :: Monoid r
    => NodeLoc -> M.State OutPort r -> Command State ()
modifyOutPortsForNode nl action = withJustM (getExpressionNode nl)
    $ \n -> mapM_
        (flip modifyOutPort action)
        (OutPortRef nl . view Port.portId <$> outPortsList n)

getPortDefault :: InPortRef -> Command State (Maybe PortDefault)
getPortDefault portRef = maybe
    Nothing
    (\mayPort -> mayPort ^? state . _WithDefault)
    <$> (NE.getPort portRef <$> getNodeEditor)

getLocalFunctions :: Command State [Text]
getLocalFunctions = do
    functionsNames <- toList . Set.fromList
        . fmap (view Port.name) . concatMap outPortsList <$> getAllNodes
    searcherMode <- fmap2 (view Searcher.mode) $ getSearcher
    let lambdaArgsNames = case searcherMode of
            Just (Searcher.Node _ (Searcher.NodeModeInfo _ _ argNames _) _)
                -> argNames
            _   -> []
    return $ functionsNames <> lambdaArgsNames


removeNodesVisualizationsData :: [NodeLoc] -> Command State ()
removeNodesVisualizationsData nls = modifyNodeEditor $ do
    NE.nodeVisualizations %= \nvMap -> foldl (flip Map.delete) nvMap nls
    NE.visualizationsBackup . NE.backupMap
        %= \backupMap -> foldl (flip Map.delete) backupMap nls


-- removeNodesVisualizations :: [NodeLoc] -> Command State ()
-- removeNodesVisualizations nls = 

-- removeBackupForNodes :: [NodeLoc] -> Command State ()
-- removeBackupForNodes nls = modifyNodeEditor
--     $ NE.visualizationsBackup . NE.backupMap
--         %= \backupMap -> foldl (flip Map.delete) backupMap nls

updateVisualizers :: Maybe FilePath -> Command State ()
updateVisualizers mayProjectVisPath = do
    internalVisPath <- liftIO $ JS.getInternalVisualizersLibraryPath
    lunaVisPath     <- liftIO $ JS.getLunaVisualizersLibraryPath
    modifyNodeEditor $ NE.visualizersLibPaths
        .= VisualizersPaths internalVisPath lunaVisPath mayProjectVisPath

    internalVisMap
        <- liftIO $ fromJSInternalVisualizersMap <$> JS.mkInternalVisualizersMap
    lunaVisMap <- liftIO $ Map.mapKeys
        (flip Visualization.VisualizerId Visualization.LunaVisualizer)
        . fromJSVisualizersMap <$> JS.mkLunaVisualizersMap
    projectVisMap  <- case mayProjectVisPath of
        Nothing -> return mempty
        Just fp -> liftIO $ Map.mapKeys
            (flip Visualization.VisualizerId Visualization.ProjectVisualizer)
            . fromJSVisualizersMap <$> JS.mkProjectVisualizersMap fp
    Global.visualizers         .= Map.union lunaVisMap projectVisMap
    Global.internalVisualizers .= internalVisMap

getNodeVisualizations :: NodeLoc -> Command State (Maybe Visualization.NodeVisualizations)
getNodeVisualizations nl = view (NE.nodeVisualizations . at nl) <$> getNodeEditor

-- getVisualizersForType :: TypeRep
--     -> Command State (Maybe (Visualization.Visualizer, Map Visualization.VisualizerId Visualization.VisualizerPath))
-- getVisualizersForType tpe = do
--     mayPrefVis   <- HashMap.lookup tpe <$> use preferedVisualizers
--     visualizers' <- use visualizers >>= applyType tpe
--     let mayFirstVisInMap
--             = fmap (uncurry Visualization.Visualizer) . listToMaybe $ Map.toList visualizers'
--         fromPrefVis vis = maybe
--             mayFirstVisInMap
--             (Just . Visualization.Visualizer (vis ^. Visualization.visualizerId))
--             $ Map.lookup (vis ^. Visualization.visualizerId) visualizers'
--         mayDefVis = maybe mayFirstVisInMap fromPrefVis mayPrefVis
--     print("DEF VIS", mayDefVis, visualizers')
--     return $ if isNothing mayDefVis || Map.null visualizers'
--         then Nothing
--         else (, visualizers') <$> mayDefVis

-- addVisualizationForNode :: NodeLoc -> Command State ()
-- addVisualizationForNode nl = undefined
 -- withJustM (getExpressionNode nl) $ \n -> do
 --    mayVisualizer <- if ExpressionNode.returnsError n then getErrorVisualizer
 --        else
 --            if ExpressionNode.hasData n
 --            && isJust (n ^. ExpressionNode.defaultVisualizer)
 --                then return $ n ^. ExpressionNode.defaultVisualizer
 --                else getPlaceholderVisualizer
 --    withJust mayVisualizer $ \visualizer' -> do
 --        let newVis = Visualization.Visualization
 --                Visualization.Ready (Just $ visualizer' ^. visualizerId)
 --            updateNodeVisualizations _ nodeVis
 --                = nodeVis & Visualization.idleVisualizations %~ (newVis :)
 --        modifyNodeEditor $ NE.nodeVisualizations %= Map.insertWith
 --            updateNodeVisualizations
 --            nl
 --            (Visualization.NodeVisualizations def [newVis] def)

-- updateDefaultVisualizer :: NodeLoc -> Maybe Visualization.Visualizer -> Bool
--     -> Command State ()
-- updateDefaultVisualizer nl vis sendAsRequest = withJustM (getExpressionNode nl)
--     $ \n -> when (n ^. ExpressionNode.defaultVisualizer /= vis) $ do
--         modifyExpressionNode nl $ ExpressionNode.defaultVisualizer .= vis
--         withJustM (getNodeMeta nl) $ \nm -> if sendAsRequest
--             then Batch.setNodesMeta        $ Map.fromList [(nl, nm)]
--             else Batch.sendNodesMetaUpdate $ Map.fromList [(nl, nm)]

-- recoverVisualizations :: NodeLoc -> Command State [Visualization.VisualizationId]
-- recoverVisualizations nl = undefined
-- getNodeVisualizations nl >>= \case
--     Nothing -> return def
--     Just nodeVis -> do
--         let (ready, outdated) = partition
--                 ((== Visualization.Ready)
--                     . view Visualization.visualizationStatus)
--                 $ nodeVis ^. Visualization.idleVisualizations
--         running <- fmap Map.fromList . forM ready $ \vis -> do
--             visId <- getUUID
--             liftIO $ JS.registerVisualizerFrame visId
--             return (visId, Visualization.RunningVisualization
--                 visId
--                 def
--                 $ vis ^. Visualization.idleVisualizerProperties)
--         modifyNodeEditor $ do
--             NE.nodeVisualizations . ix nl . Visualization.visualizations
--                 %= Map.union running
--             NE.nodeVisualizations . ix nl . Visualization.idleVisualizations
--                 .= outdated
--         maybe
--             def
--             (Map.keys . view Visualization.visualizations)
--             <$> getNodeVisualizations nl


-- stopVisualizationsForNode :: NodeLoc -> Command State ()
-- stopVisualizationsForNode nl = undefined
    -- = modifyNodeEditor $ NE.nodeVisualizations . at nl %= stopVisualizations

-- startReadyVisualizations :: NodeLoc -> Command State ()
-- startReadyVisualizations nl = undefined
-- do
    -- print ("START RESPAWN")
    -- mayVisBackup <- getVisualizationBackup nl
    -- mayNodeVis   <- getNodeVisualizations  nl
    -- print ("START READY VIS", mayVisBackup, mayNodeVis)
    -- let activateWith newNodeVis vis =
    --         if vis ^. visualizationStatus == Outdated
    --             then return
    --                 $ newNodeVis & Visualization.idleVisualizations %~ (vis:)
    --             else do
    --                 uuid <- getUUID
    --                 liftIO $ JS.registerVisualizerFrame uuid
    --                 return $ newNodeVis & Visualization.visualizations
    --                     %~ Map.insert uuid (RunningVisualization
    --                         uuid
    --                         def
    --                         $ vis ^. idleVisualizerProperties)
    --     updateVis nodeVis (Just backup) = do
    --         nVis <- foldlM activateWith (nodeVis & idleVisualizations .~ def)
    --             $ nodeVis ^. idleVisualizations
    --         modifyNodeEditor $ NE.nodeVisualizations . at nl ?= nVis
    --         setVisualizationData nl backup True
    --     updateVis nodeVis Nothing = do
    --         noVisForType <- maybe
    --             (return False)
    --             (fmap isNothing . getVisualizersForType)
    --             =<< getExpressionNodeType nl
    --         let msg = if noVisForType then noVisMsg else awaitingDataMsg
    --         updateVis nodeVis . Just $ NE.MessageBackup msg
    -- updateVisualizationsForNode nl
    -- withJust mayNodeVis $ flip updateVis mayVisBackup

-- updatePlaceholderVisualization :: NodeLoc -> Command State [Visualization.VisualizationId]
-- updatePlaceholderVisualization nl = getExpressionNode nl >>= \case
--     Nothing -> stopVisualizationsForNode nl >> pure mempty
--     Just n  -> if n ^. ExpressionNode.visEnabled
--         then setPlaceholderVisualization nl
--         else stopVisualizationsForNode nl >> pure mempty

-- setPlaceholderVisualization :: NodeLoc -> Command State [Visualization.VisualizationId]
-- setPlaceholderVisualization nl = undefined
-- getExpressionNode nl >>= \mayN -> do
--     case mayN of
--         Nothing -> stopVisualizationsForNode nl
--         Just n  -> getPlaceholderVisualizer >>= \case
--             Nothing             -> stopVisualizationsForNode nl
--             Just placeholderVis -> do
--                 mayVis <- maybe
--                     (return def)
--                     getVisualizersForType
--                     $ n ^. ExpressionNode.nodeType
--                 modifyNodeEditor $ NE.nodeVisualizations %= \visMap -> do
--                     let prevVis = maybe
--                             def
--                             (^. Visualization.visualizations)
--                             $ Map.lookup nl visMap
--                         running = Map.filter
--                             ((placeholderVis ==)
--                                 . view (Visualization.visualizerProperties
--                                 . Visualization.runningVisualizer)
--                             )
--                             prevVis
--                         idle = if Map.null running
--                             then [Visualization.IdleVisualization
--                                 Visualization.Ready
--                                 def
--                                 ]
--                             else []
--                         visualizers' = maybe def snd mayVis
--                     Map.insert
--                         nl
--                         (Visualization.NodeVisualizations
--                             running
--                             idle
--                             visualizers'
--                         )
--                         visMap
--     recoverVisualizations nl

-- updateErrorVisualization :: NodeLoc -> Command State [Visualization.VisualizationId]
-- updateErrorVisualization nl = getExpressionNode nl >>= \case
--     Nothing -> stopVisualizationsForNode nl >> pure mempty
--     Just n  -> if n ^. ExpressionNode.visEnabled
--         then setErrorVisualization nl
--         else stopVisualizationsForNode nl >> pure mempty

-- setErrorVisualization :: NodeLoc -> Command State [Visualization.VisualizationId]
-- setErrorVisualization nl = undefined 
-- getExpressionNode nl >>= \mayN -> do
--     if isNothing mayN
--         then stopVisualizationsForNode nl
--         else getErrorVisualizer >>= \case
--             Nothing       -> stopVisualizationsForNode nl
--             Just errorVis
--                 -> do
--                     print "SET ERROR VIS"
--                     print mayN
--                     modifyNodeEditor $ NE.nodeVisualizations %= \visMap -> do
--                         let prevVis = maybe
--                                 def
--                                 (^. Visualization.visualizations)
--                                 $ Map.lookup nl visMap
--                             running = Map.filter
--                                 ((errorVis ==)
--                                     . view (Visualization.visualizerProperties
--                                     . Visualization.runningVisualizer)
--                                 )
--                                 prevVis
--                             idle = if Map.null running
--                                 then [Visualization.IdleVisualization
--                                     Visualization.Ready
--                                     def
--                                     ]
--                                 else []
--                         Map.insert
--                             nl
--                             (Visualization.NodeVisualizations running idle def)
--                             visMap
--     recoverVisualizations nl

-- updateVisualizationsForNode :: NodeLoc -> Command State [VisualizationId]
-- updateVisualizationsForNode nl
--     = getExpressionNode nl >>= maybe clearVis updateWithNode where
--         clearVis         = stopVisualizationsForNode nl >> return def
--         updateWithNode n = if ExpressionNode.returnsError n
--                 then setErrorVisualization nl
--             else if not $ n ^. ExpressionNode.visEnabled
--                 then clearVis
--                 else do
--                     mayVis <- maybe
--                         (return def)
--                         getVisualizersForType
--                         $ n ^. ExpressionNode.nodeType
--                     if not (ExpressionNode.hasData n) || isNothing mayVis
--                         then setPlaceholderVisualization nl
--                         else withJust mayVis $ updateWithVis n
--         updateWithVis n vis = do
--             modifyNodeEditor $ NE.nodeVisualizations %= \visMap -> do
--                 let prevVis = maybe
--                         def
--                         (^. Visualization.visualizations)
--                         $ Map.lookup nl visMap
--                     running = Map.filter
--                         (not . has (Visualization.visualizerProperties
--                             . Visualization.runningVisualizer
--                             . visualizerId . visualizerType
--                             . _InternalVisualizer)
--                         )
--                         prevVis
--                     idle = if Map.null running
--                         then [Visualization.IdleVisualization
--                             Visualization.Ready
--                             $ Just $ (fst vis) ^. visualizerId
--                             ]
--                         else []
--                 Map.insert
--                     nl
--                     (Visualization.NodeVisualizations
--                         running
--                         idle
--                         $ snd vis
--                     )
--                     visMap
--             updateDefaultVisualizer nl (Just $ fst vis) False
--             recoverVisualizations nl


-- updatePreferedVisualizer :: TypeRep -> Visualization.Visualizer -> Command State ()
-- updatePreferedVisualizer tpe vis = preferedVisualizers . at tpe ?= vis

setNodeProfilingData :: NodeLoc -> Integer -> Command State ()
setNodeProfilingData nl t
    = modifyExpressionNode nl $ ExpressionNode.execTime ?= t

isNewData :: NodeLoc -> VisualizationBackup -> Command State Bool
isNewData nl vp = (Just vp /=) <$> getVisualizationBackup nl


resetSuccessors :: NodeLoc -> Command State ()
resetSuccessors nl = do
    outConnections <- filter (\c -> c ^. srcNodeLoc == nl) <$> getConnections
    let successors = view dstNodeLoc <$> outConnections
    whenM (resetNode nl) $ do
        mapM_ resetSuccessors successors

resetNode :: NodeLoc -> Command State Bool
resetNode nl = do
    maySuccess <- modifyExpressionNode nl $ do
        let resetPort = Port.valueType .~ TStar
        oldInPorts  <- use ExpressionNode.inPorts
        oldOutPorts <- use ExpressionNode.inPorts
        ExpressionNode.inPorts  %= fmap resetPort
        ExpressionNode.outPorts %= fmap resetPort
        ExpressionNode.value .= def
        newInPorts  <- use ExpressionNode.inPorts
        newOutPorts <- use ExpressionNode.inPorts
        return . First . Just
            $ (oldInPorts /= newInPorts) && (oldOutPorts /= newOutPorts)
    setVisualizationData
        nl
        (NE.MessageBackup Visualization.awaitingDataMsg)
        True
    return $ fromMaybe False $ getFirst maySuccess





getExpressionNodeType :: NodeLoc -> Command State (Maybe TypeRep)
getExpressionNodeType
    = fmap (maybe def (view ExpressionNode.nodeType)) . getExpressionNode

getVisualizationBackup :: NodeLoc -> Command State (Maybe VisualizationBackup)
getVisualizationBackup nl = Map.lookup nl <$> getVisualizationsBackupMap

getVisualizationsBackupMap :: Command State (Map NodeLoc VisualizationBackup)
getVisualizationsBackupMap
    = view (NE.visualizationsBackup . NE.backupMap) <$> getNodeEditor

getPlaceholderVisualizer :: Command State (Maybe Visualization.Visualizer)
getPlaceholderVisualizer
    = fmap (Visualization.Visualizer Visualization.placeholderVisId) . Map.lookup Visualization.placeholderVisId
    <$> use internalVisualizers

getErrorVisualizer :: Command State (Maybe Visualization.Visualizer)
getErrorVisualizer = fmap (Visualization.Visualizer Visualization.errorVisId) . Map.lookup Visualization.errorVisId
    <$> use internalVisualizers

hideDataVisualizations :: NodeLoc -> Command State ()
hideDataVisualizations nl = modifyNodeEditor
    $ NE.nodeVisualizations . ix nl . Visualization.dataVisualizations . traverse . Visualization.mode .= Visualization.Hidden

setErrorVisualization :: NodeLoc -> Command State ()
setErrorVisualization nl = do
    hideDataVisualizations nl
    mayErrVisualizer <- getErrorVisualizer
    mayPrevVis <- maybe Nothing (view Visualization.errorVisualization)
        <$> getNodeVisualizations nl
    let mayPrevVisualizer = view Visualization.visualizer <$> mayPrevVis
        mayCurrentVisualizer = listToMaybe
            $ catMaybes [mayErrVisualizer, mayPrevVisualizer]
    withJust mayCurrentVisualizer $ \visualizer' -> do
        errVis <- updatedVisualization def visualizer' Nothing mayPrevVis
        modifyNodeEditor
            $ NE.nodeVisualizations . ix nl . Visualization.errorVisualization ?= errVis

getActiveIframesIds :: NodeLoc -> Command State [Visualization.IframeId]
getActiveIframesIds nl = view Visualization.iframeId
    `fmap2` getActiveVisualizations nl where
        getActiveVisualizations nl = maybe
            mempty
            (Map.elems . view Visualization.visualizations)
            <$> getNodeVisualizations nl

setVisualizationData :: NodeLoc -> VisualizationBackup -> Bool
    -> Command State ()
setVisualizationData nl backup@(NE.ValueBackup val) overwrite
    = whenM ((overwrite ||) <$> isNewData nl backup) $ do
        modifyNodeEditor
            $ NE.visualizationsBackup . NE.backupMap . at nl ?= backup
        iframeIds <- getActiveIframesIds nl
        withJustM (maybe def toConstructorRep <$> getExpressionNodeType nl)
            $ \cRep -> liftIO . forM_ iframeIds
                $ \visId -> JS.sendVisualizationData visId cRep val
setVisualizationData nl backup@(NE.StreamBackup values) _overwrite@True
    = whenM (isNewData nl backup) $ do
        modifyNodeEditor
            $ NE.visualizationsBackup . NE.backupMap . at nl ?= backup
        iframeIds <- getActiveIframesIds nl
        withJustM (maybe def toConstructorRep <$> getExpressionNodeType nl)
            $ \cRep -> liftIO . forM_ iframeIds
                $ \visId -> JS.notifyStreamRestart visId cRep $ reverse values
setVisualizationData nl (NE.StreamBackup values) _overwrite@False = do
    modifyNodeEditor
        $ NE.visualizationsBackup . NE.backupMap . ix nl . NE._StreamBackup
            %= (values <>)
    iframeIds <- getActiveIframesIds nl
    liftIO . forM_ iframeIds $ forM_ values . JS.sendStreamDatapoint
setVisualizationData nl backup@(NE.MessageBackup msg) overwrite
    = whenM ((overwrite ||) <$> isNewData nl backup) $ do
        modifyNodeEditor
            $ NE.visualizationsBackup . NE.backupMap . at nl ?= backup
        print =<< (view NE.nodeVisualizations <$> getNodeEditor)
        iframeIds <- getActiveIframesIds nl
        print =<< (view NE.nodeVisualizations <$> getNodeEditor)
        liftIO . forM_ iframeIds $ flip JS.sendInternalData msg
setVisualizationData nl backup@(NE.ErrorBackup msg) overwrite
    = whenM ((overwrite ||) <$> isNewData nl backup) $ do
        modifyNodeEditor
            $ NE.visualizationsBackup . NE.backupMap . at nl ?= backup
        iframeIds <- getActiveIframesIds nl
        liftIO . forM_ iframeIds $ flip JS.sendInternalData msg


mkNodeVisualizations :: NodeLoc -> Command State (Visualization.NodeVisualizations)
mkNodeVisualizations nl = do
    visMap <- getVisualizersMapForNode nl
    let nv = Visualization.NodeVisualizations mempty Nothing visMap
    modifyNodeEditor $ NE.nodeVisualizations . at nl ?= nv
    withJustM (getExpressionNode nl) $ \n ->
        when (ExpressionNode.returnsError n) $ setErrorVisualization nl
    fromMaybe nv <$> getNodeVisualizations nl
        
addNewVisualization :: NodeLoc -> VisualizationBackup -> Command State ()
addNewVisualization nl visBackup = withJustM (getExpressionNode nl) $ \n -> do
    nodeVis <- getNodeVisualizations nl >>= \case
        Nothing -> mkNodeVisualizations nl
        Just nv -> pure nv
    mayPrefVis <- getPreferedVisualizer nl
    let mayPrefVisId = view Visualization.visualizerId <$> mayPrefVis
    mayCurrentVis <-
        if has (_ShortValue) (n ^. ExpressionNode.value) && isJust mayPrefVis
            then pure mayPrefVis
            else getPlaceholderVisualizer
    withJust mayCurrentVis $ \cv -> do
        let modeFromNode n = if n ^. ExpressionNode.visEnabled
                then def else Visualization.Hidden
        mode' <- maybe Visualization.Hidden modeFromNode <$> getExpressionNode nl
        vis <- mkVisualization mode' cv mayPrefVisId
        modifyNodeEditor $ NE.nodeVisualizations . ix nl . Visualization.dataVisualizations
            %= Map.insert (vis ^. Visualization.visualizationId) vis

setVisualizer :: Visualization.Visualizer -> Visualization.Visualization -> Command State Visualization.Visualization
setVisualizer visualizer' vis = if vis ^. Visualization.visualizer == visualizer' then pure vis else do
    let visId = vis ^. Visualization.visualizationId
        newMaySelected = if visualizer' ^. Visualization.visualizerId . Visualization.visualizerType /= Visualization.InternalVisualizer
            then Just $ visualizer' ^. Visualization.visualizerId
            else vis ^. Visualization.selectedVisualizerId
    mkVisualizationWithId
        visId
        (vis ^. Visualization.mode)
        visualizer'
        newMaySelected

setPlaceholderVisualizers :: NodeLoc -> Command State ()
setPlaceholderVisualizers nl = withJustM getPlaceholderVisualizer
    $ \placeholder -> updateDataVisualizations nl (setVisualizer placeholder)

updateDataVisualizations :: NodeLoc -> (Visualization.Visualization -> Command State Visualization.Visualization) -> Command State ()
updateDataVisualizations nl updateFunction = do
    visList <- maybe mempty (view Visualization.dataVisualizations) <$> getNodeVisualizations nl
    newVisList <- forM visList updateFunction
    modifyNodeEditor $ NE.nodeVisualizations . ix nl . Visualization.dataVisualizations .= newVisList
    

updateNodeVisualizers :: NodeLoc -> Command State ()
updateNodeVisualizers nl = getVisualizersMapForNode nl >>= \visMap -> do
    modifyNodeEditor $ NE.nodeVisualizations . ix nl . Visualization.visualizers .= visMap
    let isValid visualizer'
            = Map.member (visualizer' ^. Visualization.visualizerId) visMap
        updateWithVisualizer prefVis = updateDataVisualizations nl $ \vis
            -> if isValid (vis ^. Visualization.visualizer)
                then pure vis
                else setVisualizer prefVis vis
    maybe
        (setPlaceholderVisualizers nl)
        updateWithVisualizer
        =<< getPreferedVisualizer nl
        
getPreferedVisualizer :: NodeLoc -> Command State (Maybe Visualization.Visualizer)
getPreferedVisualizer nl = do
    let findPrefVis tpe = HashMap.lookup tpe <$> use preferedVisualizers
    mayPrefVisId <- maybe (pure Nothing) findPrefVis =<< getExpressionNodeType nl
    visualizers' <- fromMaybe mempty <$> (view Visualization.visualizers `fmap2` getNodeVisualizations nl)
    let mayFirstVisInMap
            = fmap (uncurry Visualization.Visualizer) . listToMaybe $ Map.toList visualizers'
        fromPrefVis visId = maybe
            mayFirstVisInMap
            (Just . Visualization.Visualizer visId)
            $ Map.lookup visId visualizers'
    pure $ maybe mayFirstVisInMap fromPrefVis mayPrefVisId


getVisualizersMapForNode :: NodeLoc
    -> Command State (Map Visualization.VisualizerId Visualization.VisualizerPath)
getVisualizersMapForNode nl = getExpressionNodeType nl >>= getVisualizersForType where
    getVisualizersForType Nothing    = pure mempty
    getVisualizersForType (Just tpe) = use visualizers >>= applyType tpe

mkVisualization :: Visualization.Mode -> Visualization.Visualizer -> Maybe Visualization.VisualizerId -> Command State (Visualization.Visualization)
mkVisualization mode' visualizer' maySelected = getUUID >>= \visId
    -> mkVisualizationWithId visId mode' visualizer' maySelected

mkVisualizationWithId :: Visualization.VisualizationId -> Visualization.Mode -> Visualization.Visualizer -> Maybe Visualization.VisualizerId -> Command State (Visualization.Visualization)
mkVisualizationWithId visId mode' visualizer' maySelected = do
    iframeId <- getUUID
    when (mode' /= Visualization.Hidden) $ JS.registerVisualizerFrame iframeId
    pure $ Visualization.Visualization visId iframeId mode' visualizer' maySelected

updatedVisualization :: Visualization.Mode -> Visualization.Visualizer -> Maybe Visualization.VisualizerId -> Maybe Visualization.Visualization -> Command State (Visualization.Visualization)
updatedVisualization newMode newVisualizer mayNewSelected Nothing
    = mkVisualization newMode newVisualizer mayNewSelected
updatedVisualization newMode newVisualizer mayNewSelected (Just prevVis) = do
    let visId = prevVis ^. Visualization.visualizationId
    if newVisualizer /= prevVis ^. Visualization.visualizer || newMode == Visualization.Hidden || prevVis ^. Visualization.mode /= Visualization.Hidden
        then pure $ prevVis
            & Visualization.mode                 .~ newMode
            & Visualization.visualizer           .~ newVisualizer
            & Visualization.selectedVisualizerId .~ mayNewSelected
        else mkVisualizationWithId visId newMode newVisualizer mayNewSelected


setVisualizationMode :: NodeLoc -> Visualization.VisualizationId -> Visualization.Mode -> Command State ()
setVisualizationMode nl visId newMode = withJustM (getNodeVisualizations nl) $ \nv -> do
    if nv ^? Visualization.errorVisualization . _Just . Visualization.visualizationId == Just visId then do
        when (newMode /= Visualization.Hidden) $ setErrorVisualization nl
        modifyNodeEditor $ NE.nodeVisualizations . ix nl . Visualization.errorVisualization . traverse . Visualization.mode .= newMode
    else do
        let mayVis = nv ^. Visualization.dataVisualizations . at visId
        withJust mayVis $ \vis -> do
            newVis <- updatedVisualization newMode (vis ^. Visualization.visualizer) (vis ^. Visualization.selectedVisualizerId) mayVis
            modifyNodeEditor $ NE.nodeVisualizations . ix nl . Visualization.dataVisualizations . at visId ?= newVis

getDataVisualization :: NodeLoc -> Visualization.VisualizationId -> Command State (Maybe Visualization.Visualization)
getDataVisualization nl visId = maybe
    Nothing
    (view (Visualization.dataVisualizations . at visId))
    <$> getNodeVisualizations nl

getNodeVisualizerMap :: NodeLoc -> Command State (Map Visualization.VisualizerId Visualization.VisualizerPath)
getNodeVisualizerMap nl = maybe mempty (view Visualization.visualizers) <$> getNodeVisualizations nl

getNodeVisualizer :: NodeLoc -> Visualization.VisualizerId -> Command State (Maybe Visualization.Visualizer)
getNodeVisualizer nl visId = fmap (Visualization.Visualizer visId) . Map.lookup visId <$> getNodeVisualizerMap nl

setDefaultVisualizer :: NodeLoc -> Maybe Visualization.VisualizerId -> Command State ()
setDefaultVisualizer nl mayNewVis = withJustM (getExpressionNode nl) $ \n -> do
    when (n ^. ExpressionNode.defaultVisualizer /= mayNewVis) $ do
        modifyExpressionNode nl $ ExpressionNode.defaultVisualizer .= mayNewVis
        withJustM (getNodeMeta nl) $ \nm
            -> Batch.setNodesMeta $ Map.fromList [(nl, nm)]

selectVisualizer :: NodeLoc -> Visualization.VisualizationId -> Visualization.VisualizerId -> Command State ()
selectVisualizer nl visId newVisualizerId = withJustM (getDataVisualization nl visId) $ \vis -> do
    setDefaultVisualizer nl (Just newVisualizerId)
    if vis ^. Visualization.visualizer . Visualization.visualizerId . Visualization.visualizerType == Visualization.InternalVisualizer
        then modifyNodeEditor $ NE.nodeVisualizations . ix nl
            . Visualization.dataVisualizations . ix visId . Visualization.selectedVisualizerId ?= newVisualizerId
        else withJustM (getNodeVisualizer nl newVisualizerId) $ \newVisualizer -> do
            newVis <- mkVisualizationWithId
                visId
                (vis ^. Visualization.mode)
                newVisualizer
                $ Just newVisualizerId
            modifyNodeEditor $ NE.nodeVisualizations . ix nl . Visualization.dataVisualizations . at visId ?= newVis
            withJustM (getVisualizationBackup nl) $ \visBackup ->
                setVisualizationData nl visBackup True

-- updateDefaultVisualizer :: NodeLoc -> Maybe Visualization.Visualizer -> Bool
--     -> Command State ()
-- updateDefaultVisualizer nl vis sendAsRequest = withJustM (getExpressionNode nl)
--     $ \n -> when (n ^. ExpressionNode.defaultVisualizer /= vis) $ do
--         modifyExpressionNode nl $ ExpressionNode.defaultVisualizer .= vis
--         withJustM (getNodeMeta nl) $ \nm -> if sendAsRequest
--             then Batch.setNodesMeta        $ Map.fromList [(nl, nm)]
--             else Batch.sendNodesMetaUpdate $ Map.fromList [(nl, nm)]

        -- if vis ^. Visualization.visualizer . Visualization.visualizerId . Visualization.visualizerType = Visualization.InternalVisualizer
        --     then modifyNodeEditor $

-- getCurrentVisualizer :: Maybe Visualizer -> ExpressionNode -> Command State (Maybe Visualizer)
-- getCurrentVisualizer mayPrefVis n = case n ^. ExpressionNode.value of
--     AwaitingTypecheck -> getPlaceholderVisualizer
--     AwaitingData      -> getPlaceholderVisualizer
--     ShortValue {}     -> pure mayPrefVis
--     Error {}          -> getErrorVisualizer

-- mkNodeVisualizations :: ExpressionNode -> VisualizerProperies -> Map VisualizerId VisualizerPath -> Command State NodeVisualizations
-- mkNodeVisualizations n visProp visMap = do
--     runningVis <- if not $ n ^. ExpressionNode.visEnabled
--         then pure []
--         else do
--             uuid <- getUUID
--             pure [RunningVisualization uuid def visProp]
--     let idleVis = if n ^. ExpressionNode.visEnabled
--         then []
--         else [IdleVisualization Ready visProp]
--     NodeVisualizations runningVis idleVis visMap

-- unionNodeVisualizations :: NodeVisualizations

-- addNewVisualization :: NodeLoc -> VisualizationBackup
--     -> Command State (Maybe VisualizationId)
-- addNewVisualization nl visBackup = getExpressionNode nl $ \case
--     Nothing -> pure Nothing
--     Just n -> do
--         (mayPrefVis, visMap) <- getVisualizersForType' =<<  nl
--         uuid                 <- getUUID
--         mayCurrentVisualizer <- getCurrentVisualizer mayPrefVis
--         case mayCurrentVisualizer of
--             Nothing -> pure Nothing
--             Just currentVis -> do
--                 newNodeVis <- mkNodeVisualizations n (VisualizerProperties currentVis mayPrefVis) visMap
