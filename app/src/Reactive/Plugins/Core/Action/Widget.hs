{-# LANGUAGE OverloadedStrings #-}

module Reactive.Plugins.Core.Action.Widget where

import           Utils.PreludePlus
import           Utils.Vector

import           Object.Object
import           Object.Widget
import           Object.Node
import           Event.Mouse    hiding      ( Event, WithObjects )
import qualified Event.Mouse    as Mouse
import qualified Event.Window   as Window
import           Event.Event
import           Event.WithObjects
import           Reactive.Plugins.Core.Action.Action
import qualified Reactive.Plugins.Core.Action.State.Global       as Global
import qualified Reactive.Plugins.Core.Action.State.UIRegistry   as UIRegistry
import qualified Object.Widget.Button as Button
import           GHCJS.Prim
import           Data.IntMap.Lazy (IntMap)
import qualified Data.IntMap.Lazy as IntMap
import           Utils.CtxDynamic
import           Data.Set (Set)
import qualified Data.Set as Set

data Action = MouseMoving   { _pos   :: MousePosition }
            | MousePressed  { _pos   :: MousePosition, _button :: MouseButton }
            | MouseReleased { _pos   :: MousePosition, _button :: MouseButton }
            | ApplyUpdates  { _actions :: [WidgetUIUpdate] }

makeLenses ''Action

instance PrettyPrinter Action where
    display _ = "WidgetAction"

toAction :: Event Node -> Maybe Action
toAction (Mouse (Mouse.Event tpe pos button _)) = case tpe of
    Mouse.Moved     -> Just $ MouseMoving   pos
    Mouse.Pressed   -> Just $ MousePressed  pos $ toMouseButton button
    Mouse.Released  -> Just $ MouseReleased pos $ toMouseButton button
toAction _           = Nothing


onlyIfOver :: MousePosition -> DisplayObject -> (MousePosition -> DisplayObject -> WidgetUpdate) -> WidgetUpdate
onlyIfOver pos widget f = if isOver pos widget then f pos widget else (Nothing, widget)

handleEvents :: Action -> [WidgetUIUpdate] -> DisplayObject -> ([WidgetUIUpdate], DisplayObject)
handleEvents action uiUpdates widget = (uiUpdate:uiUpdates, newWidget) where
    (uiUpdate, newWidget) = case action of
        MouseMoving   pos        -> onlyIfOver pos widget onMouseMove
        MousePressed  pos button -> onlyIfOver pos widget $ onMousePressed  button
        MouseReleased pos button -> onlyIfOver pos widget $ onMouseReleased button


instance ActionStateUpdater Action where
    execSt newActionCandidate oldState = case newAction of
            Just action -> ActionUI newAction newState
            Nothing     -> ActionUI  NoAction newState
        where
        newAction               = Just $ ApplyUpdates uiUpdates
        newState                = oldState & Global.uiRegistry . UIRegistry.widgets .~ newWidgets
                                           & Global.uiRegistry . UIRegistry.widgetOver .~ widgetOver
        (uiUpdates', newWidgets') = IntMap.mapAccum (handleEvents newActionCandidate) [] oldWidgets
        oldWidgetOver          = oldState ^. Global.uiRegistry . UIRegistry.widgetOver
        widgetOver :: Maybe UIRegistry.WidgetId
        uiUpdates :: [WidgetUIUpdate]
        newWidgets :: UIRegistry.WidgetMap
        (uiUpdates, newWidgets, widgetOver) = case newActionCandidate of
            MouseMoving pos -> (uiOutOverUpdates, newOutOverWidgets, widgetOver) where
                maybeOver :: Maybe DisplayObject
                maybeOver = find (isOver pos) (IntMap.elems newWidgets')
                widgetOver :: Maybe UIRegistry.WidgetId
                widgetOver =  objectId <$> maybeOver
                (uiOutOverUpdates, newOutOverWidgets)      = if oldWidgetOver /= widgetOver then (ooUpdates, ooWidgets)
                                                                                              else (uiUpdates', newWidgets') where
                                                                                                   (newUIUpdates, newOutWidgets) = maybe (uiUpdates', newWidgets') id $ do
                                                                                                       oid <- oldWidgetOver
                                                                                                       oldOut <- IntMap.lookup oid newWidgets'
                                                                                                       (a, w) <- Just $ onMouseOut pos oldOut
                                                                                                       Just (a:uiUpdates', IntMap.insert oid w newWidgets')
                                                                                                   (ooUpdates, ooWidgets) = maybe (newUIUpdates, newOutWidgets) id $ do
                                                                                                       oid <- widgetOver
                                                                                                       oldOver <- IntMap.lookup oid newOutWidgets
                                                                                                       (a, w) <- Just $ onMouseOver pos oldOver
                                                                                                       Just (a:newUIUpdates, IntMap.insert oid w newOutWidgets)

            _               -> (uiUpdates', newWidgets', oldWidgetOver)


        oldWidgets              = oldState ^. Global.uiRegistry . UIRegistry.widgets



instance ActionUIUpdater Action where
    updateUI (WithState action state) = case action of
        ApplyUpdates actions -> do
            putStrLn $ "len " <> (show $ length actions)
            putStrLn $ show $ IntMap.size $ state ^. Global.uiRegistry . UIRegistry.widgets
            sequence_ $ catMaybes actions
