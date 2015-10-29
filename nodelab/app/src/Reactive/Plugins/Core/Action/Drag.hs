module Reactive.Plugins.Core.Action.Drag where

import           Utils.PreludePlus
import           Utils.Vector
import           Utils.Angle

import qualified JS.Bindings    as UI
import qualified JS.NodeGraph   as UI

import           Object.Object
import           Object.Node
import           Object.UITypes

import           Event.Keyboard hiding      (Event)
import qualified Event.Keyboard as Keyboard
import           Event.Mouse    hiding      (Event, WithObjects)
import qualified Event.Mouse    as Mouse
import           Event.Event
import           Event.WithObjects

import           Reactive.Plugins.Core.Action
import           Reactive.Commands.Graph
import           Reactive.Commands.Command (execCommand)
import           Reactive.State.Drag
import qualified Reactive.State.Graph      as Graph
import qualified Reactive.State.Selection  as Selection
import qualified Reactive.State.Camera     as Camera
import qualified Reactive.State.Global     as Global
import           Reactive.State.UnderCursor

import qualified Data.IntMap.Lazy        as IntMap
import qualified BatchConnector.Commands as BatchCmd
import           Batch.Workspace         (Workspace)

data ActionType = StartDrag
                | Moving
                | Dragging
                | StopDrag
                deriving (Eq, Show)

data Action = DragAction { _actionType :: ActionType
                         , _actionPos  :: Vector2 Int
                         }
              deriving (Eq, Show)


makeLenses ''Action


instance PrettyPrinter ActionType where
    display = show

instance PrettyPrinter Action where
    display (DragAction tpe point) = "dA(" <> display tpe <> " " <> display point <> ")"


toAction :: Event -> Global.State -> UnderCursor -> Maybe Action
toAction (Mouse (Mouse.Event tpe pos button keyMods _)) state underCursor = case button of
    LeftButton         -> case tpe of
        Mouse.Pressed  -> if dragAllowed then case keyMods of
                                             (KeyMods False False False False) -> Just (DragAction StartDrag pos)
                                             _                                 -> Nothing
                                         else Nothing
        Mouse.Released -> if isDragging then Just (DragAction StopDrag pos) else Nothing
        Mouse.Moved    -> if isDragging then Just (DragAction Moving   pos) else Nothing
        _              -> Nothing
    _                  -> Nothing
    where dragAllowed   = not . null $ underCursor ^. nodesUnderCursor
          isDragging    = isJust $ state ^. Global.drag . history
toAction _ _ _ = Nothing

moveNodes :: Double -> Vector2 Int -> [NodeId] -> NodesMap -> NodesMap
moveNodes factor delta selection = fmap $ \node -> if elem (node ^. nodeId) selection then node & nodePos +~ deltaWs else node where
    deltaWs = deltaToWs factor delta

deltaToWs :: Double -> Vector2 Int -> Vector2 Double
deltaToWs factor delta = (/ factor) . fromIntegral <$> delta

instance ActionStateUpdater Action where
    execSt newActionCandidate oldState = case newAction of
        Just action -> ActionUI newAction newState'
        Nothing     -> ActionUI  NoAction newState'
        where
        oldDrag                          = oldState ^. Global.drag . history
        oldGraph                         = oldState ^. Global.graph
        oldNodesMap                      = Graph.getNodesMap oldGraph
        newGraph                         = Graph.updateNodes newNodesMap oldGraph
        selection                        = oldState ^. Global.selection . Selection.nodeIds
        camFactor                        = oldState ^. Global.camera . Camera.camera . Camera.factor
        emptySelection                   = IntMap.null oldNodesMap
        newState                         = oldState & Global.iteration       +~ 1
                                                    & Global.drag  . history .~ newDrag
                                                    & Global.graph           .~ newGraph
        (_, newState')                   = execCommand (updatePortAngles >> updateConnections) newState
        newAction                        = case newActionCandidate of
            DragAction Moving pt        -> case oldDrag of
                Nothing                 -> Nothing
                _                       -> Just $ DragAction Dragging pt
            DragAction StopDrag pt      -> case oldDrag of
                Nothing                 -> Nothing
                Just oldDragState       -> if (oldDragState ^. dragStartPos) /= pt then Just newActionCandidate
                                                                                   else Nothing
            _                           -> Just newActionCandidate
        newNodesMap                      = case newActionCandidate of
            DragAction tpe point        -> case tpe of
                Moving                  -> case oldDrag of
                    Just oldDragState   -> moveNodes camFactor (delta oldDragState) selection oldNodesMap
                    Nothing             -> oldNodesMap
                StopDrag                -> case oldDrag of
                    Just oldDragState   -> moveNodes camFactor (delta oldDragState) selection oldNodesMap
                    Nothing             -> oldNodesMap
                _                       -> oldNodesMap
                where delta oldDragState = point - (oldDragState ^. dragCurrentPos)
        newDrag                          = case newActionCandidate of
            DragAction tpe point        -> case tpe of
                StartDrag               -> Just $ DragHistory point point point
                Moving                  -> if emptySelection then Nothing else case oldDrag of
                    Just oldDragState   -> Just $ DragHistory startPos prevPos point
                        where startPos   = oldDragState ^. dragStartPos
                              prevPos    = oldDragState ^. dragCurrentPos
                    Nothing             -> Nothing
                StopDrag                -> Nothing


instance ActionUIUpdater Action where
    updateUI (WithState action state) = case action of
        DragAction tpe pt            -> case tpe of
            StartDrag                -> return ()
            Moving                   -> return ()
            Dragging                 -> do
                                            moveNodesUI selNodes
                                            fst $ execCommand updatePortAnglesUI state
                                            fst $ execCommand updateConnectionsUI state
            StopDrag                 -> do
                                            moveNodesUI selNodes
                                            updateNodesBatch workspace selNodes
                                            fst $ execCommand updatePortAnglesUI state
                                            fst $ execCommand updateConnectionsUI state
            where
                workspace             = state ^. Global.workspace
                nodesMap              = Graph.getNodesMap       $ state ^. Global.graph
                connectionsMap        = Graph.getConnectionsMap $ state ^. Global.graph
                selNodeIds            = state ^. Global.selection . Selection.nodeIds
                selNodes              = IntMap.filter (\node -> node ^. nodeId `elem` selNodeIds) nodesMap
                factor                = state ^. Global.camera . Camera.camera . Camera.factor
                deltaWs               = case state ^. Global.drag . history of
                    Just dragState   -> deltaToWs factor delta where
                        delta         = dragState ^. dragCurrentPos - dragState ^. dragStartPos
                    Nothing          -> Vector2 0.0 0.0


updateNodesBatch :: Workspace -> NodesMap -> IO ()
updateNodesBatch workspace nodesMap = BatchCmd.updateNodes workspace $ IntMap.elems nodesMap
