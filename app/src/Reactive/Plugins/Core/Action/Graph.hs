module Reactive.Plugins.Core.Action.Graph where

import           Prelude       hiding       ( mapM_, forM_ )
import           Data.Foldable              ( mapM_, forM_ )
import           Control.Lens
import           Data.Default
import           Data.Maybe
import           Data.Monoid
import           System.Mem

import           JS.Bindings
import           Object.Object
import qualified Object.Node    as Node     ( position )
import           Object.Node    hiding      ( position )
import           Event.Keyboard hiding      ( Event )
import qualified Event.Keyboard as Keyboard
import           Event.Mouse    hiding      ( Event, WithObjects )
import qualified Event.Mouse    as Mouse
import           Event.Event
import           Utils.Wrapper
import           Utils.PrettyPrinter
import           Reactive.Plugins.Core.Action.Action

data ActionType = SelectNew
                | Focus
                | ToggleOn
                | ToggleOff
                deriving (Eq, Show)

data DragActionType = StartDrag
                    | Moving
                    | StopDrag
                    deriving (Eq, Show)

data DragPos = DragPos { _dragStartPos    :: Point
                       , _dragPreviousPos :: Point
                       , _dragCurrentPos  :: Point
                       } deriving (Eq, Show)

data DragState = DragState { _dragState :: Maybe DragPos } deriving (Eq, Show)

data Action = SelectAction { _actionType :: ActionType
                           , _actionNode :: Node
                           }
            | DragAction   { _dragActionType :: DragActionType
                           , _dragActionPos  :: Point
                           }
            | UnselectAll
            deriving (Eq, Show)

data State = State { _drag  :: DragState
                   , _nodes :: [Node]
                   } deriving (Eq, Show)


type ActionState = WithState (Maybe Action) State

makeLenses ''Action
makeLenses ''State
makeLenses ''DragPos
makeLenses ''DragState

instance Default DragState where
    def = DragState def

instance Default State where
    def = State def def


instance PrettyPrinter ActionType where
    display = show

instance PrettyPrinter DragActionType where
    display = show

instance PrettyPrinter Action where
    display UnselectAll = "sa( UnselectAll )"
    display (SelectAction tpe node)  = "sa( " <> display tpe <> " " <> display node <> " )"
    display (DragAction   tpe point) = "da( " <> display tpe <> " " <> display point <> " )"

instance PrettyPrinter State where
    display (State dragging nodes) = display dragging <> " " <> display nodes

instance PrettyPrinter DragPos where
    display (DragPos start prev current) = display start <> " " <> display prev <> " " <> display current

instance PrettyPrinter DragState where
    display (DragState dragState) = "d( " <> display dragState <> " )"


moveNodes :: Point -> [Node] -> [Node]
moveNodes delta = fmap $ Node.position +~ delta

removeNode :: Node -> [Node] -> [Node]
removeNode nodeToRemove = filter (\node -> node ^. ident /= nodeToRemove ^. ident)
-- removeNode (Node identRem _ _) = filter (\(Node ident _ _) -> ident /= identRem)

accumActionState :: Action -> ActionState -> ActionState
accumActionState newAction oldActionState = WithState (Just newAction) $ State newDrag newNodes
    where
    oldAction                      = oldActionState ^. action
    oldState                       = oldActionState ^. state
    oldDrag                        = oldState ^. drag
    oldDragState                   = oldDrag  ^. dragState
    oldNodes                       = oldState ^. nodes
    newNodes                       = case newAction of
        UnselectAll               -> []
        SelectAction tpe node     -> case tpe of
            SelectNew             -> [newNode]
            Focus                 -> newNode : oldFilteredNodes
            ToggleOn              -> newNode : oldFilteredNodes
            ToggleOff             -> oldFilteredNodes
            where oldFilteredNodes = removeNode node oldNodes
                  newNode          = select node
        DragAction tpe point      -> case tpe of
            Moving                -> case oldDragState of
                Just oldDragState -> moveNodes delta oldNodes
                    where prevPos  = oldDragState ^. dragCurrentPos
                          delta    = point - prevPos
                Nothing           -> oldNodes
            _                     -> oldNodes
        -- _                         -> oldNodes
    newDrag                        = case newAction of
        DragAction tpe point      -> case tpe of
            StartDrag             -> DragState $ Just $ DragPos point point point
            Moving                -> case oldDragState of
                Just oldDragState -> DragState $ Just $ DragPos startPos prevPos point
                    where startPos = oldDragState ^. dragStartPos
                          prevPos  = oldDragState ^. dragCurrentPos
                Nothing           -> DragState Nothing
            StopDrag              -> DragState Nothing
        _                         -> DragState Nothing


keyboardToAction :: Keyboard.Event -> Maybe Action
keyboardToAction event = case event ^. char of
    'u' -> Just UnselectAll
    _   -> Nothing

mouseToDragAction :: Mouse.WithObjects Node -> Maybe Action
mouseToDragAction eventWithObjects = case mouseEvent ^. tpe of
    Mouse.Pressed  -> if isNoNode then Nothing
                                  else case mouseKeyMods of
                                       (KeyMods False False False False) -> Just (DragAction StartDrag mousePosition)
                                       _                                 -> Nothing
    Mouse.Released -> Just (DragAction StopDrag mousePosition)
    Mouse.Moved    -> Just (DragAction Moving   mousePosition)
    where mouseEvent    = eventWithObjects ^. event
          mouseKeyMods  = mouseEvent ^. keyMods
          mousePosition = mouseEvent ^. position
          isNoNode      = null $ eventWithObjects ^. objects


mouseToSelectAction :: Mouse.WithObjects Node -> Maybe Action
mouseToSelectAction eventWithObjects = case mouseEvent ^. tpe of
    Mouse.Pressed -> if isNoNode then case mouseKeyMods of
                                    (KeyMods False False False False) -> Just UnselectAll
                                    _                                 -> Nothing
                                 else case mouseKeyMods of
                                    (KeyMods False False False False) -> Just (SelectAction selectActionType node)
                                    (KeyMods False False True  False) -> Just (SelectAction toggleActionType node)
                                    _                                 -> Nothing
    _             -> Nothing
    where mouseEvent       = eventWithObjects ^. event
          mouseKeyMods     = mouseEvent ^. keyMods
          isNoNode         = null $ eventWithObjects ^. objects
          node             = unwrap . head $ eventWithObjects ^. objects
          selectActionType = if node ^. selected then Focus
                                                 else SelectNew
          toggleActionType = if node ^. selected then ToggleOff
                                                 else ToggleOn


updateNodesUI :: ActionState -> IO ()
updateNodesUI (WithState maybeAction state) = case maybeAction of
    Nothing           -> return ()
    Just action       -> case action of
        SelectAction tpe (Node nodeId _ _) -> case tpe of
            SelectNew -> unselectAllNodes
                      >> setNodeFocused nodeId
            Focus     -> setNodeFocused nodeId
            ToggleOn  -> setNodeFocused nodeId
            ToggleOff -> setNodeUnselected nodeId
                      >> mapM_ setNodeFocused topNodeId
        UnselectAll   -> unselectAllNodes
        DragAction tpe pt -> case tpe of
            StartDrag -> return ()
            Moving    -> moveNodesUI selectedNodes
            StopDrag  -> return ()
        where selectedNodes = state ^. nodes
              topNodeId = selectedNodes ^? ix 0 . ident


moveNodeUI :: Node -> IO ()
moveNodeUI (Node ident _ (Point x y)) = dragNode ident x y

moveNodesUI :: [Node] -> IO ()
moveNodesUI nodes  = mapM_ moveNodeUI nodes
                  -- >> performGC
