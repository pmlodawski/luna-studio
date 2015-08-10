{-# LANGUAGE OverloadedStrings #-}

module Reactive.Plugins.Core.Action.Widget where

import           Utils.PreludePlus
import           Utils.Vector

import           Object.Object
import           Object.Widget
import           Object.Node
import           Event.Mouse    hiding      (Event, WithObjects)
import qualified Event.Mouse    as Mouse
import qualified Event.Window   as Window
import           Event.Event
import           Event.WithObjects
import           Reactive.Plugins.Core.Action.Action
import qualified Reactive.Plugins.Core.Action.State.Global       as Global
import           Reactive.Plugins.Core.Action.State.UIRegistry   (WidgetMap)
import qualified Reactive.Plugins.Core.Action.State.UIRegistry   as UIRegistry
import qualified Object.Widget.Button as Button
import qualified Object.Widget.Slider as Slider
import           ThreeJS.Button ()
import           GHCJS.Prim
import           Data.IntMap.Lazy (IntMap)
import qualified Data.IntMap.Lazy as IntMap
import           Utils.CtxDynamic
import           Data.Set (Set)
import qualified Data.Set as Set
import           Debug.Trace

data Action = MouseAction   { _event   :: Mouse.Event }
            | ApplyUpdates  { _actions :: [WidgetUIUpdate] }

makeLenses ''Action

instance PrettyPrinter Action where
    display _ = "WidgetAction"

toAction :: Event Node -> Maybe Action
toAction (Mouse m) = Just $ MouseAction m
toAction _         = Nothing

handleGeneric :: Mouse.Event -> UIRegistry.State -> Maybe (WidgetUIUpdate, UIRegistry.State)
handleGeneric (Mouse.Event eventType _ button _ (Just (EventWidget widgetId pos))) registry = do
    widget                <- (UIRegistry.lookup widgetId registry) :: Maybe DisplayObject
    (uiUpdate, newWidget) <- return $ case eventType of
        Mouse.Moved       -> onMouseMove    button pos widget
        Mouse.Pressed     -> onMousePress   button pos widget
        Mouse.Released    -> onMouseRelease button pos widget
        Mouse.Clicked     -> onClick               pos widget
        Mouse.DblClicked  -> onDblClick            pos widget
    let newRegistry = UIRegistry.update newWidget registry
    return (uiUpdate, registry)
handleGeneric _  _        = Nothing

triggerHandler :: Maybe WidgetId -> (DisplayObject -> WidgetUpdate) -> UIRegistry.State -> Maybe (WidgetUIUpdate, UIRegistry.State)
triggerHandler maybeOid handler state = do
    oid                     <- maybeOid
    oldWidget               <- UIRegistry.lookup oid state
    let (action, newWidget) = handler oldWidget
    let newState            = UIRegistry.update newWidget state
    return (action, newState)


instance ActionStateUpdater Action where
    execSt (MouseAction mouseEvent) oldState = case newAction of
            Just action -> ActionUI newAction newState
            Nothing     -> ActionUI  NoAction newState
        where
        newAction                = Just $ ApplyUpdates uiUpdates
        newState                 = oldState &  Global.uiRegistry .~ newRegistry
        oldRegistry              = oldState ^. Global.uiRegistry
        oldWidgetOver            = oldState ^. Global.uiRegistry . UIRegistry.widgetOver
        (uiUpdates, newRegistry) = UIRegistry.sequenceUpdates [ Just $ setWidgetOver
                                                              , handleMouseOut
                                                              , handleMouseOver
                                                              , Just $ (handleGeneric mouseEvent)
                                                              ] oldRegistry
        (handleMouseOver, handleMouseOut) = case mouseEvent of
            Mouse.Event Mouse.Moved _ _ _ evWd -> case widgetOverChanged of
                                        True   -> (Just $ triggerHandler oldWidgetOver onMouseOut
                                                  ,Just $ triggerHandler newWidgetOver onMouseOver
                                                  )
                                        False  -> (Nothing, Nothing)
            _             -> (Nothing, Nothing)
        widgetOverChanged = oldWidgetOver /= newWidgetOver
        newWidgetOver     = case mouseEvent of
            Mouse.Event Mouse.Moved _ _ _ evWd -> (^. Mouse.widgetId) <$> evWd
            _                                  -> oldWidgetOver
        setWidgetOver state = Just $ (Nothing, state & UIRegistry.widgetOver .~ newWidgetOver)

instance ActionUIUpdater Action where
    updateUI (WithState (ApplyUpdates actions) state) = do
        sequence_ $ catMaybes actions
