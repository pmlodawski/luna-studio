module Reactive.Plugins.Core.Action.State.UIRegistry where


import           Utils.PreludePlus hiding (children)
import           Utils.Vector

import           Object.Widget
import           Object.Widget.Scene

import           Data.IntMap.Lazy (IntMap)
import qualified Data.IntMap.Lazy as IntMap

import           Object.UITypes
import           Utils.CtxDynamic

import qualified Control.Monad.State as MState

data WidgetFile  = WidgetFile { _widget   :: DisplayObject
                              , _parent   :: Maybe WidgetId
                              , _children :: [WidgetId]
                              }

makeLenses ''WidgetFile

type WidgetMap = IntMap WidgetFile

data State = State { _widgets         :: WidgetMap
                   , _widgetOver      :: Maybe WidgetId
                   , _dragState       :: Maybe DragState
                   , _focusedWidget   :: Maybe WidgetId
                   }

makeLenses ''State

instance Eq State where
    a == b = (IntMap.size $ a ^. widgets) == (IntMap.size $ a ^. widgets)
instance Show State where
    show a = show $ IntMap.size $ a ^. widgets

defaultWidgets = [(sceneInterfaceId, sceneInterface), (sceneInterfaceId, sceneGraph)] where
    sceneInterface = WidgetFile (toCtxDynamic $ Scene sceneInterfaceId) Nothing []
    sceneGraph     = WidgetFile (toCtxDynamic $ Scene sceneGraphId    ) Nothing []

instance Default State where
    def = State (fromList defaultWidgets) def def def

instance PrettyPrinter State where
    display (State widgets wover dragging focus) =
           "dWd("        <> show (IntMap.keys widgets)
        <> " over: "     <> show wover
        <> " dragging: " <> show dragging
        <> " focus: "    <> show focus
        <> ")"

lookup :: WidgetId -> State -> Maybe DisplayObject
lookup idx state = (^. widget) <$> IntMap.lookup idx (state ^. widgets)

register :: DisplayObjectClass a => WidgetId -> a -> State -> (a, State)
register parent a state = (aWithId, state & widgets .~ newWidgets') where
    newWidgets'   = IntMap.insert parent newParent newWidgets
    newWidgets    = IntMap.insert newId wfile oldWidgets
    newId         = generateId state
    aWithId       = a & objectIdLens .~ newId
    oldWidgets    = state ^. widgets
    (Just oldParent) = IntMap.lookup parent oldWidgets
    newParent     = oldParent & children .~ (newId:(oldParent ^. children))
    wfile         = WidgetFile (toCtxDynamic aWithId) (Just parent) []

type UIState a = MState.State (State, [Maybe (IO ())]) a

registerM :: DisplayObjectClass a => WidgetId -> a -> UIState a
registerM parent widget = do
    (st, acts) <- MState.get
    let (widget', st') = register parent widget st
    MState.put (st', acts)
    return widget'

update :: DisplayObject -> State -> State
update a state = state & widgets .~ newWidgets where
    oldFile       = IntMap.lookup (objectId a) oldWidgets
    newWidgets    = case oldFile of
        Just file -> IntMap.insert (objectId a) (file & widget .~ a) oldWidgets
        Nothing   -> oldWidgets
    oldWidgets = state ^. widgets

updateM :: DisplayObjectClass a => a -> UIState ()
updateM widget = do
    (st, acts) <- MState.get
    let st' = update (toCtxDynamic widget) st
    MState.put (st', acts)
    return ()

uiAction act = do
    (st, acts) <- MState.get
    MState.put (st, (Just act):acts)

unregister :: DisplayObjectClass a => a -> State -> State
unregister a state = state & widgets .~ newWidgets where
    newWidgets = IntMap.delete (objectId a) oldWidgets
    oldWidgets = state ^. widgets

registerAll :: DisplayObjectClass a => WidgetId -> [a] -> State -> ([a], State)
registerAll parent a state = foldl reg ([], state) a where
    reg (acc, st) a = (newA:acc, newSt) where
        (newA, newSt) = register parent a st

unregisterAll :: DisplayObjectClass a => [a] -> State -> State
unregisterAll a state = foldl (flip unregister) state a

generateIds :: Int -> State -> [WidgetId]
generateIds count state = [startId..startId + count] where
    startId = if IntMap.size (state ^. widgets) == 0 then 1
                                                     else maxId + 1 where (maxId, _) = IntMap.findMax (state ^. widgets)
generateId :: State -> WidgetId
generateId state = if IntMap.size (state ^. widgets) == 0 then 1
                                                          else maxId + 1 where (maxId, _) = IntMap.findMax (state ^. widgets)

replaceAll :: DisplayObjectClass a => WidgetId -> [a] -> [a] -> State -> ([a], State)
replaceAll parent remove add state = registerAll parent add $ unregisterAll remove state

sequenceUpdates :: [Maybe (State -> Maybe (WidgetUIUpdate, State))]
                -> State -> ([WidgetUIUpdate], State)
sequenceUpdates ops input = foldl applyOp ([], input) ops where
    applyOp (updates, input) op = fromMaybe (updates, input) $ do
        justOp           <- op
        (update, output) <- justOp input
        return (update:updates, output)
