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

type MouseMoveHandler     a = MouseButton -> Position -> a -> (a, IO ())
type MousePressedHandler  a = MouseButton -> Position -> a -> (a, IO ())
type MouseReleasedHandler a = MouseButton -> Position -> a -> (a, IO ())
type MouseOverHandler     a =                            a -> (a, IO ())
type MouseOutHandler      a =                            a -> (a, IO ())
type ClickHandler         a =                Position -> a -> (a, IO ())
type DblClickHandler      a =                Position -> a -> (a, IO ())
type KeyUpHandler         a = Char                    -> a -> (a, IO ())
type KeyDownHandler       a = Char                    -> a -> (a, IO ())
type KeyPressedHandler    a = Char                    -> a -> (a, IO ())

data UIHandlers a  = UIHandlers { _mouseMove     :: [MouseMoveHandler      a]
                                , _mousePressed  :: [MousePressedHandler   a]
                                , _mouseReleased :: [MouseReleasedHandler  a]
                                , _mouseOver     :: [MouseOverHandler      a]
                                , _mouseOut      :: [MouseOutHandler       a]
                                , _click         :: [ClickHandler          a]
                                , _dblClick      :: [DblClickHandler       a]
                                , _keyUp         :: [KeyUpHandler          a]
                                , _keyDown       :: [KeyDownHandler        a]
                                , _keyPressed    :: [KeyPressedHandler     a]
                                }

makeLenses ''UIHandlers

instance Default (UIHandlers a) where def = UIHandlers [] [] [] [] [] [] [] [] [] []

instance Monoid  (UIHandlers a) where
    mempty = def
    mappend (UIHandlers a  b  c  d  e  f  g  h  i  j )
            (UIHandlers a' b' c' d' e' f' g' h' i' j') = UIHandlers (a <> a')
                                                                    (b <> b')
                                                                    (c <> c')
                                                                    (d <> d')
                                                                    (e <> e')
                                                                    (f <> f')
                                                                    (g <> g')
                                                                    (h <> h')
                                                                    (i <> i')
                                                                    (j <> j')

data WidgetFile a  = WidgetFile { _widget   :: DisplayObject
                                , _parent   :: Maybe WidgetId
                                , _children :: [WidgetId]
                                , _handlers :: UIHandlers a
                                }

makeLenses ''WidgetFile

type WidgetMap a = IntMap (WidgetFile a)

data State a = State { _widgets         :: WidgetMap a
                     , _widgetOver      :: Maybe WidgetId
                     , _dragState       :: Maybe DragState
                     , _focusedWidget   :: Maybe WidgetId
                     }

makeLenses ''State

instance Eq (State a) where
    a == b = (IntMap.size $ a ^. widgets) == (IntMap.size $ a ^. widgets)
instance Show (State a) where
    show a = show $ IntMap.size $ a ^. widgets

defaultWidgets :: [(WidgetId, WidgetFile a)]
defaultWidgets = [(sceneInterfaceId, sceneInterface), (sceneInterfaceId, sceneGraph)] where
    sceneInterface = WidgetFile (toCtxDynamic $ Scene sceneInterfaceId) Nothing [] def
    sceneGraph     = WidgetFile (toCtxDynamic $ Scene sceneGraphId    ) Nothing [] def

instance Default (State a) where
    def = State (fromList defaultWidgets) def def def

instance PrettyPrinter (State a) where
    display (State widgets wover dragging focus) =
           "dWd("        <> show (IntMap.keys widgets)
        <> " over: "     <> show wover
        <> " dragging: " <> show dragging
        <> " focus: "    <> show focus
        <> ")"

lookup :: WidgetId -> (State a) -> Maybe DisplayObject
lookup idx state = (^. widget) <$> IntMap.lookup idx (state ^. widgets)

lookupHandlers :: WidgetId -> (State a) -> Maybe (UIHandlers a)
lookupHandlers idx state = (^. handlers) <$> IntMap.lookup idx (state ^. widgets)

register :: DisplayObjectClass a => WidgetId -> a -> UIHandlers b -> State b -> (a, State b)
register parent a handlers state = (aWithId, state & widgets .~ newWidgets') where
    newWidgets'   = IntMap.insert parent newParent newWidgets
    newWidgets    = IntMap.insert newId  wfile     oldWidgets
    newId         = generateId state
    aWithId       = a & objectIdLens .~ newId
    oldWidgets    = state ^. widgets
    (Just oldParent) = IntMap.lookup parent oldWidgets
    newParent     = oldParent & children .~ (newId:(oldParent ^. children))
    wfile         = WidgetFile (toCtxDynamic aWithId) (Just parent) [] handlers

type UIState a b = MState.State (State b, [Maybe (IO ())]) a

registerM :: DisplayObjectClass a => WidgetId -> a -> UIHandlers b -> UIState a b
registerM parent widget handlers = do
    (st, acts) <- MState.get
    let (widget', st') = register parent widget handlers st
    MState.put (st', acts)
    return widget'

update :: DisplayObject -> State a -> State a
update a state = state & widgets .~ newWidgets where
    oldFile       = IntMap.lookup (objectId a) oldWidgets
    newWidgets    = case oldFile of
        Just file -> IntMap.insert (objectId a) (file & widget .~ a) oldWidgets
        Nothing   -> oldWidgets
    oldWidgets = state ^. widgets

updateFile :: WidgetId -> (WidgetFile a -> WidgetFile a) -> State a -> State a
updateFile oid mutator state = state & widgets .~ newWidgets where
    oldFile       = IntMap.lookup oid oldWidgets
    newWidgets    = case oldFile of
        Just file -> IntMap.insert oid (mutator file) oldWidgets
        Nothing   -> oldWidgets
    oldWidgets = state ^. widgets

updateM :: DisplayObjectClass a => a -> UIState () b
updateM widget = do
    (st, acts) <- MState.get
    let st' = update (toCtxDynamic widget) st
    MState.put (st', acts)
    return ()

uiAction act = do
    (st, acts) <- MState.get
    MState.put (st, (Just act):acts)

registerHandler :: WidgetId -> (UIHandlers a -> UIHandlers a) -> State a -> State a
registerHandler oid mutator state = updateFile oid fileMutator state where
    fileMutator file = file & handlers %~ mutator

unregister :: DisplayObjectClass a => a -> State b -> State b -- TODO: Remove children from parent
unregister a state = state & widgets .~ newWidgets where
    newWidgets = IntMap.delete (objectId a) oldWidgets
    oldWidgets = state ^. widgets

registerAll :: DisplayObjectClass a => WidgetId -> [a] -> State b -> ([a], State b)
registerAll parent a state = foldl reg ([], state) a where
    reg (acc, st) a = (newA:acc, newSt) where
        (newA, newSt) = register parent a def st

unregisterAll :: DisplayObjectClass a => [a] -> State b -> State b
unregisterAll a state = foldl (flip unregister) state a

generateIds :: Int -> State b -> [WidgetId]
generateIds count state = [startId..startId + count] where
    startId = if IntMap.size (state ^. widgets) == 0 then 1
                                                     else maxId + 1 where (maxId, _) = IntMap.findMax (state ^. widgets)
generateId :: State b -> WidgetId
generateId state = if IntMap.size (state ^. widgets) == 0 then 1
                                                          else maxId + 1 where (maxId, _) = IntMap.findMax (state ^. widgets)

replaceAll :: DisplayObjectClass a => WidgetId -> [a] -> [a] -> State b -> ([a], State b)
replaceAll parent remove add state = registerAll parent add $ unregisterAll remove state

sequenceUpdates :: [Maybe (State b -> Maybe (WidgetUIUpdate, State b))]
                -> State b -> ([WidgetUIUpdate], State b)
sequenceUpdates ops input = foldl applyOp ([], input) ops where
    applyOp (updates, input) op = fromMaybe (updates, input) $ do
        justOp           <- op
        (update, output) <- justOp input
        return (update:updates, output)
