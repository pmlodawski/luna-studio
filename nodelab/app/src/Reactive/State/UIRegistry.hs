{-# LANGUAGE OverloadedStrings #-}

module Reactive.State.UIRegistry where

import           Utils.PreludePlus hiding (children, lookup, (.=))
import           Utils.Vector

import           Object.Widget
import           Object.Widget.Scene
import           Object.Widget.Connection
import UI.Widget.Connection ()

import           Data.IntMap.Lazy (IntMap)
import qualified Data.Text.Lazy as Text
import qualified Data.IntMap.Lazy as IntMap
import           Data.Tuple       (swap)

import           Object.UITypes
import           Utils.CtxDynamic

import qualified Control.Monad.State     as MState
import qualified Control.Monad.Trans.RWS as RWS
import           Control.Monad.Trans.RWS (RWS)

import           Reactive.Commands.Command (Command, performIO, pureCommand)

import Data.Aeson (ToJSON, toJSON, object, (.=), Value)
import           Utils.Aeson (intMapToJSON)
import           Data.HMap.Lazy (HTMap)


sceneInterfaceId, sceneGraphId, currentConnectionId :: Int
sceneInterfaceId    = 1
sceneGraphId        = 2
currentConnectionId = 3

type WidgetMap = IntMap (WidgetFile DisplayObject)

data State = State { _widgets         :: WidgetMap
                     , _widgetOver      :: Maybe WidgetId
                     , _dragState       :: Maybe DragState
                     , _focusedWidget   :: Maybe WidgetId
                     } deriving (Generic)

makeLenses ''State

instance ToJSON State where
    toJSON st = object [ "_widgets"       .= (intMapToJSON $ st ^. widgets)
                       , "_widgetOver"    .= (toJSON $ st ^. widgetOver)
                       , "_dragState"     .= (toJSON $ st ^. dragState)
                       , "_focusedWidget" .= (toJSON $ st ^. focusedWidget)
                       ]

instance Eq State where
    a == b = (IntMap.size $ a ^. widgets) == (IntMap.size $ a ^. widgets)
instance Show State where
    show a = show $ IntMap.size $ a ^. widgets

defaultWidgets :: [(WidgetId, WidgetFile DisplayObject)]
defaultWidgets = [ (sceneInterfaceId,     sceneInterface)
                 , (sceneGraphId,         sceneGraph)
                 , (currentConnectionId,  currentConnection)
                 ] where
    sceneInterface    = WidgetFile sceneInterfaceId    (toCtxDynamic $ Scene) Nothing [] def
    sceneGraph        = WidgetFile sceneGraphId        (toCtxDynamic $ Scene) Nothing [] def
    currentConnection = WidgetFile currentConnectionId (toCtxDynamic $ CurrentConnection False def def def) Nothing [] def

instance Default State where
    def = State (fromList defaultWidgets) def def def

lookup :: WidgetId -> State -> Maybe (WidgetFile DisplayObject)
lookup idx state = IntMap.lookup idx (state ^. widgets)

lookupM :: WidgetId -> Command State (Maybe (WidgetFile DisplayObject))
lookupM id = preuse $ widgets . ix id

lookupHandlers :: WidgetId -> State -> Maybe HTMap
lookupHandlers idx state = (^. handlers) <$> IntMap.lookup idx (state ^. widgets)

register :: DisplayObjectClass a => WidgetId -> a -> HTMap -> State -> (WidgetFile a, State)
register parent a handlers state = (widgetFile, state & widgets .~ newWidgets') where
    newWidgets'   = IntMap.insert parent newParent   newWidgets
    newWidgets    = IntMap.insert newId  dynamicFile oldWidgets
    newId         = generateId state
    oldWidgets    = state ^. widgets
    (Just oldParent) = IntMap.lookup parent oldWidgets
    newParent     = oldParent & children .~ (newId:(oldParent ^. children))
    dynamicFile   = WidgetFile newId (toCtxDynamic a) (Just parent) [] handlers
    widgetFile    = WidgetFile newId a (Just parent) [] handlers

registerM :: DisplayObjectClass a => WidgetId -> a -> HTMap -> Command State (WidgetFile a)
registerM = MState.state .:. register

update :: WidgetId -> DisplayObject -> State -> State -- redefine in context of updateFile
update oid a state  = state & widgets .~ newWidgets where
    oldFile        = IntMap.lookup oid oldWidgets
    newWidgets     = case oldFile of
        Just file -> IntMap.insert oid (file & widget .~ a) oldWidgets
        Nothing   -> oldWidgets
    oldWidgets     = state ^. widgets

updateFile :: WidgetId -> (WidgetFile DisplayObject -> WidgetFile DisplayObject) -> State -> State
updateFile oid mutator state = state & widgets .~ newWidgets where
    oldFile       = IntMap.lookup oid oldWidgets
    newWidgets    = case oldFile of
        Just file -> IntMap.insert oid (mutator file) oldWidgets
        Nothing   -> oldWidgets
    oldWidgets = state ^. widgets

updateM :: DisplayObjectClass a => WidgetId -> a -> Command State ()
updateM id a = pureCommand $ update id (toCtxDynamic a)

updateWidgetM :: DisplayObjectClass a => WidgetId -> (a -> a) -> Command State a
updateWidgetM id fun = do
    maybeFile   <- lookupTypedM id
    let file     = fromMaybe (error "updateWidgetM: invalidType") maybeFile
        newWidget  = fun $ file ^. widget

    updateM id newWidget

    return newWidget

-- registerHandler :: WidgetId -> (UIHandlers a -> UIHandlers a) -> State a -> State a
-- registerHandler oid mutator state = updateFile oid fileMutator state where
--     fileMutator file = file & handlers %~ mutator

unregisterRWS :: WidgetId -> RWS () [WidgetId] State ()
unregisterRWS oid = do
    widget <- RWS.gets $ lookup oid
    case widget of
        Just file -> sequence_ $ unregisterRWS <$> (file ^. children)
        Nothing   -> return ()
    RWS.modify (widgets %~ IntMap.delete oid)
    RWS.tell [oid]

unregister :: WidgetId -> State -> ([WidgetId], State)
unregister oid = swap . RWS.execRWS (unregisterRWS oid) ()

unregisterM :: WidgetId -> Command State [WidgetId]
unregisterM = MState.state . unregister

registerAll :: DisplayObjectClass a => WidgetId -> [a] -> State-> ([WidgetFile a], State)
registerAll parent a state = foldl reg ([], state) a where
    reg (acc, st) a = (newA:acc, newSt) where
        (newA, newSt) = register parent a def st

unregisterAll :: [WidgetId] -> State -> ([WidgetId], State)
unregisterAll oids = swap . RWS.execRWS (sequence $ unregisterRWS <$> oids) ()

unregisterAllM :: [WidgetId] -> Command State [WidgetId]
unregisterAllM = MState.state . unregisterAll

unregisterAll_ :: [WidgetId] -> State -> State
unregisterAll_ = snd .: unregisterAll

generateIds :: Int -> State -> [WidgetId]
generateIds count state = [startId..startId + count] where
    startId = if IntMap.size (state ^. widgets) == 0 then 1
                                                     else maxId + 1 where (maxId, _) = IntMap.findMax (state ^. widgets)
generateId :: State -> WidgetId
generateId state = if IntMap.size (state ^. widgets) == 0 then 1
                                                          else maxId + 1 where (maxId, _) = IntMap.findMax (state ^. widgets)

replaceAll :: DisplayObjectClass a => WidgetId -> [WidgetId] -> [a] -> State -> ([WidgetFile a], State)
replaceAll parent remove add state = registerAll parent add (unregisterAll_ remove state)

lookupAll :: DisplayObjectClass a => State -> [WidgetFile a]
lookupAll state = foldl process mempty objects where
    process acc obj = case fromCtxDynamic $ obj ^. widget of
        Just model -> (obj & widget .~ model):acc
        Nothing    -> acc
    objects = IntMap.elems $ state ^. widgets

lookupAllM :: DisplayObjectClass a => Command State [WidgetFile a]
lookupAllM = MState.gets lookupAll

lookupTyped :: DisplayObjectClass a => WidgetId -> State -> Maybe (WidgetFile a)
lookupTyped idx state = do
    object  <- lookup idx state
    model   <- fromCtxDynamic $ object ^. widget
    return $ object & widget .~ model

lookupTypedM :: DisplayObjectClass a => WidgetId -> Command State (Maybe (WidgetFile a))
lookupTypedM ix = MState.gets $ lookupTyped ix
