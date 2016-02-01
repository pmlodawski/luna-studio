{-# LANGUAGE OverloadedStrings #-}

module UI.Handlers.List where

import           Utils.PreludePlus

import           Data.HMap.Lazy                (HTMap)
import qualified Data.Text.Lazy                as Text
import           Utils.Vector

import           Object.LunaValue              (AnyLunaValue (..), createValueWidget)
import           Object.Widget                 (WidgetId, CompositeWidget, createWidget, updateWidget, ResizableWidget, resizeWidget)

import qualified Object.Widget.Button          as Button
import qualified Object.Widget.Group           as Group
import qualified Object.Widget.Label           as Label
import           Object.Widget.List            (List (..))
import qualified Object.Widget.List            as List
import           Reactive.Commands.Command     (Command)
import qualified Reactive.Commands.UIRegistry  as UICmd
import           Reactive.State.Global         (inRegistry)
import qualified Reactive.State.Global         as Global
import           Reactive.State.UIRegistry     (addHandler)
import qualified Reactive.State.UIRegistry     as UIRegistry

import           UI.Generic                    (startDrag, takeFocus, defaultResize)
import           UI.Handlers.Group             (triggerWidgetResized)
import qualified UI.Handlers.Button            as Button
import           UI.Handlers.Generic           (ValueChangedHandler (..), triggerValueChanged)
import qualified UI.Handlers.TextBox           as TextBox
import           UI.Layout                     as Layout
import           UI.Widget.Button              ()
import           UI.Widget.Group               ()
import           UI.Widget.Label               ()
import           UI.Widget.List                ()
import           UI.Widget.Toggle              ()


deleteNth n xs = take n xs ++ drop (n+1) xs

addItemHandlers :: WidgetId -> WidgetId -> Double -> HTMap
addItemHandlers listId groupId width = addHandler (Button.ClickedHandler $ addItemHandler)
                               $ mempty where
              addItemHandler _ = inRegistry $ addNewElement listId groupId width

removeItemHandlers :: WidgetId -> WidgetId -> WidgetId -> HTMap
removeItemHandlers listId groupId rowId = addHandler (Button.ClickedHandler $ removeItemHandler)
                                        $ mempty where
    removeItemHandler _ = inRegistry $ removeElementByWidgetId listId groupId rowId

listItemHandler :: WidgetId -> WidgetId -> WidgetId -> AnyLunaValue -> WidgetId -> Command Global.State ()
listItemHandler listWidget groupId rowId val id = do
    inRegistry $ do
        items <- UICmd.children groupId
        let idx = elemIndex rowId items
        forM_ idx $ \idx -> UICmd.update_ listWidget $ List.value . ix idx .~ val
    newContent <- inRegistry $ UICmd.get listWidget List.value
    triggerValueChanged newContent listWidget

makeItem :: Bool -> WidgetId -> WidgetId -> Double -> AnyLunaValue -> Int -> Command UIRegistry.State ()
makeItem isTuple listId listGroupId width elem ix = do
    let removeButton = Button.createIcon (Vector2 20 20) "shaders/icon.minus.frag"
    groupId <- UICmd.register listGroupId Group.create def
    createValueWidget groupId elem (Text.pack $ show ix) width (addHandler (ValueChangedHandler $ listItemHandler listId listGroupId groupId) mempty)
    when (not isTuple) $ UICmd.register_ groupId removeButton (removeItemHandlers listId listGroupId groupId)
    Layout.horizontalLayout def 0.0 groupId

makeListItem  = makeItem False
makeTupleItem = makeItem True

-- makeTuple :: WidgetId -> List -> Command UIRegistry.State WidgetId
-- makeTuple parent model = do
--     contId <- UICmd.register parent model def
--     groupId <- UICmd.register contId Group.create def
--
--     let elems = (model ^. List.value) `zip` [0..]
--     forM_ elems $ uncurry $ makeTupleItem contId groupId
--
--     Layout.verticalLayout 0.0 groupId
--     Layout.verticalLayout 0.0 contId
--
--     return contId

relayout :: WidgetId -> WidgetId -> Command UIRegistry.State ()
relayout listId groupId = do
    Layout.verticalLayout def 0.0 groupId
    Layout.verticalLayout def 0.0 listId

addNewElement :: WidgetId -> WidgetId -> Double -> Command UIRegistry.State ()
addNewElement listId groupId width = do
    list   <- UICmd.get listId $ List.value
    let ix = length list
    elem <- UICmd.get listId $ List.empty
    UICmd.update listId $ List.value <>~ [elem]
    makeListItem listId groupId width elem ix

    relayout listId groupId

removeElementByWidgetId :: WidgetId -> WidgetId -> WidgetId -> Command UIRegistry.State ()
removeElementByWidgetId listId groupId id = do
    items <- UICmd.children groupId
    let ix = elemIndex id items
    forM_ ix $ \ix -> removeElement listId groupId ix

removeElement :: WidgetId -> WidgetId -> Int -> Command UIRegistry.State ()
removeElement listId groupId idx = do
    UICmd.update listId $ List.value %~ deleteNth idx
    items <- UICmd.children groupId
    UICmd.removeWidget $ fromJust $ items ^? ix idx

    relayout listId groupId

instance CompositeWidget List where
    createWidget id model = do
        let label        = Label.create (Vector2 100.0 20.0) "Param of list:"
            addButton    = Button.createIcon (Vector2 20 20) "shaders/icon.plus.frag"
            width        = model ^. List.size . x
            padding      = 10
            itemWidth    = width - addButton ^. Button.size . x - padding
            buttonIndent = itemWidth

        UICmd.register_ id label def

        groupId     <- UICmd.register id Group.create (Layout.verticalLayoutHandler def def)
        UICmd.moveX groupId padding

        addButtonId <- UICmd.register id addButton (addItemHandlers id groupId itemWidth)
        UICmd.moveX addButtonId buttonIndent

        let elems = (model ^. List.value) `zip` [0..]
        forM_ elems $ uncurry $ makeListItem id groupId itemWidth

        relayout id groupId

    updateWidget id old model = return ()

instance ResizableWidget List where
    resizeWidget id vec model = do
        defaultResize id vec model
        triggerWidgetResized id vec

