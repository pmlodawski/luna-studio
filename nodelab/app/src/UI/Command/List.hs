{-# LANGUAGE OverloadedStrings #-}

module UI.Command.List where

import           Utils.PreludePlus hiding (Choice)
import           Utils.Vector
import qualified Data.Text.Lazy as Text
import qualified Reactive.State.Global           as Global
import           Reactive.Commands.Command       (Command, performIO)

import           Reactive.State.UIRegistry     (sceneInterfaceId, sceneGraphId, addHandler)
import qualified Reactive.State.Global        as Global
import qualified Reactive.State.UIRegistry    as UIRegistry
import qualified Reactive.Commands.UIRegistry as UICmd

import           Object.Widget.List  (List(..))
import qualified Object.Widget.List   as List
import qualified Object.Widget.Button as Button
import qualified UI.Handlers.Button   as Button
import qualified Object.Widget.Group  as Group
import           Object.LunaValue
import           Object.LunaValue.Instances
import           UI.Widget.List ()

import           UI.Layout as Layout
import           Object.UITypes
import           Object.Widget
import           Reactive.State.Global (inRegistry)
import qualified Data.HMap.Lazy as HMap
import           Data.HMap.Lazy (HTMap)
import           Data.HMap.Lazy (TypeKey(..))


deleteNth n xs = take n xs ++ drop (n+1) xs


addItemHandlers :: WidgetId -> WidgetId -> HTMap
addItemHandlers listId groupId = addHandler (Button.ClickedHandler $ addItemHandler)
                               $ mempty where
              addItemHandler _ = inRegistry $ addNewElement listId groupId

removeItemHandlers :: WidgetId -> WidgetId -> WidgetId -> HTMap
removeItemHandlers listId groupId rowId = addHandler (Button.ClickedHandler $ removeItemHandler)
                                        $ mempty where
    removeItemHandler _ = inRegistry $ removeElementByWidgetId listId groupId rowId


makeListItem :: WidgetId -> WidgetId -> AnyLunaValue -> Int -> Command UIRegistry.State ()
makeListItem listId listGroupId elem ix = do
    let removeButton = Button.create (Vector2 20 20) "X"
    groupId <- UICmd.register listGroupId Group.create def
    createValueWidget groupId elem (Text.pack $ show ix) (listHandlers listId listGroupId groupId elem)
    UICmd.register groupId removeButton (removeItemHandlers listId listGroupId groupId)
    Layout.horizontalLayout 5.0 groupId


makeList :: WidgetId -> List -> Command UIRegistry.State WidgetId
makeList parent model = do
    contId <- UICmd.register parent model def
    let addButton    = Button.create (Vector2 180 20) "Add element"

    groupId <- UICmd.register contId Group.create def
    UICmd.register contId addButton (addItemHandlers contId groupId)

    let elems = (model ^. List.value) `zip` [0..]
    forM_ elems $ uncurry $ makeListItem contId groupId

    Layout.verticalLayout 0.0 groupId
    Layout.verticalLayout 0.0 contId

    return contId

addNewElement :: WidgetId -> WidgetId -> Command UIRegistry.State ()
addNewElement listId groupId = do
    list   <- UICmd.get listId $ List.value
    let ix = length list
    elem <- UICmd.get listId $ List.empty
    UICmd.update listId $ List.value <>~ [elem]
    makeListItem listId groupId elem ix

    Layout.verticalLayout 0.0 groupId
    Layout.verticalLayout 0.0 listId

removeElementByWidgetId :: WidgetId -> WidgetId -> WidgetId -> Command UIRegistry.State ()
removeElementByWidgetId listId groupId id = do
    performIO $ putStrLn $ show id
    items <- UICmd.children groupId
    let ix = elemIndex id items
    forM_ ix $ \ix -> removeElement listId groupId ix


removeElement :: WidgetId -> WidgetId -> Int -> Command UIRegistry.State ()
removeElement listId groupId idx = do
    UICmd.update listId $ List.value %~ deleteNth idx
    items <- UICmd.children groupId
    UICmd.removeWidget $ fromJust $ items ^? ix idx

    Layout.verticalLayout 0.0 groupId
    Layout.verticalLayout 0.0 listId
--
-- newtype ValueChangedHandler = ValueChangedHandler (Word -> WidgetId -> Command Global.State ())
-- valueChangedHandlerKey = TypeKey :: TypeKey ValueChangedHandler
--
-- triggerValueChanged :: LunaValue a => a -> WidgetId -> Command Global.State ()
-- triggerValueChanged new id = do
--     maybeHandler <- inRegistry $ UICmd.handler id valueChangedHandlerKey
--     forM_ maybeHandler $ \(ValueChangedHandler handler) -> handler new id
