{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Reactive.State.UIRegistry
    ( Object.Widget.State
    , Object.Widget.WidgetMap
    , dragState
    , widgets
    , registerM
    , lookupTypedM
    , updateWidgetM
    , lookupM
    , widgetOver
    , unregisterM
    , focusedWidget
    , addHandler
    , sceneGraphId
    , sceneInterfaceId
    , lookupAllM
    , mouseDownWidget
    , lookupAll
    , currentConnectionId
    , handle
    , LookupFor
    ) where

import           Utils.PreludePlus hiding (children, lookup)
import           Utils.Vector

import           Object.Widget
import           Object.Widget.Scene
import           Object.Widget.Connection
import           UI.Widget.Connection ()

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

import           Data.Aeson (ToJSON, toJSON, object, Value)
import           Utils.Aeson (intMapToJSON)
import           Data.HMap.Lazy (HTMap)
import qualified Data.HMap.Lazy as HMap

instance CompositeWidget Scene where
    createWidget _   _ = return ()
    updateWidget _ _ _ = return ()

instance ResizableWidget Scene

sceneInterfaceId, sceneGraphId, currentConnectionId :: Int
sceneInterfaceId    = 1
sceneGraphId        = 2
currentConnectionId = 3

makeLenses ''State

instance ToJSON State where
    toJSON st = object [ ("_widgets"        , intMapToJSON $ st ^. widgets  )
                       , ("_widgetOver"     , toJSON $ st ^. widgetOver     )
                       , ("_dragState"      , toJSON $ st ^. dragState      )
                       , ("_focusedWidget"  , toJSON $ st ^. focusedWidget  )
                       , ("_mouseDownWidget", toJSON $ st ^. mouseDownWidget)
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
    currentConnection = WidgetFile currentConnectionId (toCtxDynamic $ CurrentConnection False def def True def) Nothing [] def

instance Default State where
    def = State (fromList defaultWidgets) def def def def

lookup :: WidgetId -> State -> Maybe (WidgetFile DisplayObject)
lookup idx state = IntMap.lookup idx (state ^. widgets)

lookupM :: WidgetId -> Command State (Maybe (WidgetFile DisplayObject))
lookupM id = preuse $ widgets . ix id

register :: DisplayObjectClass a => WidgetId -> a -> HTMap -> State -> (WidgetFile a, State)
register parent a handlers state = (widgetFile, state & widgets .~ newWidgets') where
    newWidgets'   = IntMap.insert parent newParent   newWidgets
    newWidgets    = IntMap.insert newId  dynamicFile oldWidgets
    newId         = generateId state
    oldWidgets    = state ^. widgets
    (Just oldParent) = IntMap.lookup parent oldWidgets
    newParent     = oldParent & children .~ (oldParent ^. children) ++ [newId]
    dynamicFile   = WidgetFile newId (toCtxDynamic a) (Just parent) [] handlers
    widgetFile    = WidgetFile newId a (Just parent) [] handlers

registerM :: DisplayObjectClass a => WidgetId -> a -> HTMap -> Command State (WidgetFile a)
registerM = MState.state .:. register

updateWidgetM :: DisplayObjectClass a => WidgetId -> (a -> a) -> Command State a
updateWidgetM id fun = do
    maybeFile   <- lookupTypedM id
    let file     = fromMaybe (error "updateWidgetM: invalidType") maybeFile
        newWidget  = fun $ file ^. widget

    widgets . ix id . widget .= toCtxDynamic newWidget

    return newWidget

unregisterRWS :: WidgetId -> RWS () [WidgetId] State ()
unregisterRWS oid = do
    widget <- RWS.gets $ lookup oid
    case widget of
        Just file -> sequence_ $ unregisterRWS <$> (file ^. children)
        Nothing   -> return ()
    RWS.modify (widgets %~ IntMap.delete oid)
    RWS.tell [oid]

unregister :: WidgetId -> State -> ([WidgetId], State)
unregister oid oldState = (outWidgets, state) where
    widgetParent        = oldState ^? widgets . (ix oid) . parent
    (outWidgets, state) = case widgetParent of
        Just widgetParent -> (outWidgets, state') where
            (state, outWidgets) = RWS.execRWS (unregisterRWS oid) () oldState
            state' = case widgetParent of
                Just widgetParent -> state & widgets . ix widgetParent . children %~ delete oid
                Nothing           -> state
        Nothing     -> ([], state)

unregisterM :: WidgetId -> Command State [WidgetId]
unregisterM = MState.state . unregister

generateId :: State -> WidgetId
generateId state = if IntMap.size (state ^. widgets) == 0 then 1
                                                          else maxId + 1 where (maxId, _) = IntMap.findMax (state ^. widgets)

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

type LookupFor a = Command State (Maybe (WidgetFile a))

addHandler :: Typeable v => v -> HTMap -> HTMap
addHandler h m = HMap.insert (undefined :: HMap.TypeKey v) h m

handle :: Typeable v => v -> HTMap
handle h = HMap.insert (undefined :: HMap.TypeKey v) h mempty
