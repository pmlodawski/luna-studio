module Reactive.State.UIRegistry where


import           Utils.PreludePlus hiding (children, lookup)
import           Utils.Vector

import           Object.Widget
import           Object.Widget.Scene
import           Object.Widget.Connection
import UI.Widget.Connection ()

import           Data.IntMap.Lazy (IntMap)
import qualified Data.IntMap.Lazy as IntMap
import           Data.Tuple       (swap)

import           Object.UITypes
import           Utils.CtxDynamic

import qualified Control.Monad.State     as MState
import qualified Control.Monad.Trans.RWS as RWS
import           Control.Monad.Trans.RWS (RWS)

import           Reactive.Commands.Command (Command, performIO, pureCommand)

import Debug.Trace

sceneInterfaceId, sceneGraphId, currentConnectionId :: Int
sceneInterfaceId    = 1
sceneGraphId        = 2
currentConnectionId = 3

type WidgetMap a = IntMap (WidgetFile a DisplayObject)

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

defaultWidgets :: [(WidgetId, WidgetFile a DisplayObject)]
defaultWidgets = [ (sceneInterfaceId,     sceneInterface)
                 , (sceneGraphId,         sceneGraph)
                 , (currentConnectionId,  currentConnection)
                 ] where
    sceneInterface    = WidgetFile sceneInterfaceId    (toCtxDynamic $ Scene) Nothing [] def
    sceneGraph        = WidgetFile sceneGraphId        (toCtxDynamic $ Scene) Nothing [] def
    currentConnection = WidgetFile currentConnectionId (toCtxDynamic $ CurrentConnection False def def def) Nothing [] def

instance Default (State a) where
    def = State (fromList defaultWidgets) def def def

instance PrettyPrinter (State a) where
    display (State widgets _ wover focus) =
           "dWd("        <> show (IntMap.keys widgets)
        <> " over: "     <> show wover
        <> " focus: "    <> show focus
        <> ")"

lookup :: WidgetId -> (State a) -> Maybe (WidgetFile a DisplayObject)
lookup idx state = IntMap.lookup idx (state ^. widgets)

lookupM :: WidgetId -> Command (State a) (Maybe (WidgetFile a DisplayObject))
lookupM id = preuse $ widgets . ix id

lookupHandlers :: WidgetId -> (State a) -> Maybe (UIHandlers a)
lookupHandlers idx state = (^. handlers) <$> IntMap.lookup idx (state ^. widgets)

register :: DisplayObjectClass a => WidgetId -> a -> UIHandlers b -> State b -> (WidgetFile b a, State b)
register parent a handlers state = (widgetFile, state & widgets .~ newWidgets') where
    newWidgets'   = IntMap.insert parent newParent   newWidgets
    newWidgets    = IntMap.insert newId  dynamicFile oldWidgets
    newId         = generateId state
    oldWidgets    = state ^. widgets
    (Just oldParent) = trace (show parent) $ IntMap.lookup parent oldWidgets
    newParent     = oldParent & children .~ (newId:(oldParent ^. children))
    dynamicFile   = WidgetFile newId (toCtxDynamic a) (Just parent) [] handlers
    widgetFile    = WidgetFile newId a (Just parent) [] handlers

registerM :: DisplayObjectClass a => WidgetId -> a -> UIHandlers b -> Command (State b) (WidgetFile b a)
registerM = MState.state .:. register

update :: WidgetId -> DisplayObject -> State b -> State b -- redefine in context of updateFile
update oid a state  = state & widgets .~ newWidgets where
    oldFile        = IntMap.lookup oid oldWidgets
    newWidgets     = case oldFile of
        Just file -> IntMap.insert oid (file & widget .~ a) oldWidgets
        Nothing   -> oldWidgets
    oldWidgets     = state ^. widgets

updateFile :: WidgetId -> (WidgetFile a DisplayObject -> WidgetFile a DisplayObject) -> State a -> State a
updateFile oid mutator state = state & widgets .~ newWidgets where
    oldFile       = IntMap.lookup oid oldWidgets
    newWidgets    = case oldFile of
        Just file -> IntMap.insert oid (mutator file) oldWidgets
        Nothing   -> oldWidgets
    oldWidgets = state ^. widgets

updateM :: DisplayObjectClass a => WidgetId -> a -> Command (State b) ()
updateM id a = pureCommand $ update id (toCtxDynamic a)

updateWidgetM :: DisplayObjectClass a => WidgetId -> (a -> a) -> Command (State b) a
updateWidgetM id fun = do
    maybeFile   <- lookupTypedM id
    let file     = fromMaybe (error "updateWidgetM: invalidType") maybeFile
        newWidget  = fun $ file ^. widget

    updateM id newWidget

    return newWidget

registerHandler :: WidgetId -> (UIHandlers a -> UIHandlers a) -> State a -> State a
registerHandler oid mutator state = updateFile oid fileMutator state where
    fileMutator file = file & handlers %~ mutator

unregisterRWS :: WidgetId -> RWS () [WidgetId] (State a) ()
unregisterRWS oid = do
    widget <- RWS.gets $ lookup oid
    case widget of
        Just file -> sequence_ $ unregisterRWS <$> (file ^. children)
        Nothing   -> return ()
    RWS.modify (widgets %~ IntMap.delete oid)
    RWS.tell [oid]

unregister :: WidgetId -> State b -> ([WidgetId], State b)
unregister oid = swap . RWS.execRWS (unregisterRWS oid) ()

unregisterM :: WidgetId -> Command (State b) [WidgetId]
unregisterM = MState.state . unregister

registerAll :: DisplayObjectClass a => WidgetId -> [a] -> State b -> ([WidgetFile b a], State b)
registerAll parent a state = foldl reg ([], state) a where
    reg (acc, st) a = (newA:acc, newSt) where
        (newA, newSt) = register parent a def st

unregisterAll :: [WidgetId] -> State b -> ([WidgetId], State b)
unregisterAll oids = swap . RWS.execRWS (sequence $ unregisterRWS <$> oids) ()

unregisterAllM :: [WidgetId] -> Command (State b) [WidgetId]
unregisterAllM = MState.state . unregisterAll

unregisterAll_ :: [WidgetId] -> State b -> State b
unregisterAll_ = snd .: unregisterAll

generateIds :: Int -> State b -> [WidgetId]
generateIds count state = [startId..startId + count] where
    startId = if IntMap.size (state ^. widgets) == 0 then 1
                                                     else maxId + 1 where (maxId, _) = IntMap.findMax (state ^. widgets)
generateId :: State b -> WidgetId
generateId state = if IntMap.size (state ^. widgets) == 0 then 1
                                                          else maxId + 1 where (maxId, _) = IntMap.findMax (state ^. widgets)

replaceAll :: DisplayObjectClass a => WidgetId -> [WidgetId] -> [a] -> State b -> ([WidgetFile b a], State b)
replaceAll parent remove add state = registerAll parent add (unregisterAll_ remove state)

lookupAll :: DisplayObjectClass a => State b -> [WidgetFile b a]
lookupAll state = foldl process mempty objects where
    process acc obj = case fromCtxDynamic $ obj ^. widget of
        Just model -> (obj & widget .~ model):acc
        Nothing    -> acc
    objects = IntMap.elems $ state ^. widgets

lookupAllM :: DisplayObjectClass a => Command (State b) [WidgetFile b a]
lookupAllM = MState.gets lookupAll

lookupTyped :: DisplayObjectClass a => WidgetId -> State b -> Maybe (WidgetFile b a)
lookupTyped idx state = do
    object  <- lookup idx state
    model   <- fromCtxDynamic $ object ^. widget
    return $ object & widget .~ model

lookupTypedM :: DisplayObjectClass a => WidgetId -> Command (State b) (Maybe (WidgetFile b a))
lookupTypedM ix = MState.gets $ lookupTyped ix
