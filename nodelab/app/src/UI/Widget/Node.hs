{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module UI.Widget.Node where

import           Utils.PreludePlus
import           Utils.Vector
import           Data.JSString.Text ( lazyTextFromJSString, lazyTextToJSString )
import           GHCJS.Types        (JSVal, JSString)
import           GHCJS.Marshal.Pure (PToJSVal(..), PFromJSVal(..))
import           GHCJS.DOM.Element  (Element)
import           UI.Widget          (UIWidget(..), UIContainer(..))
import qualified UI.Registry        as UIR
import qualified Reactive.State.UIRegistry as UIRegistry
import qualified Object.Widget.Node as Model
import           Object.Widget
import           Object.UITypes
import           Event.Mouse (MouseButton(..))
import           Utils.CtxDynamic (toCtxDynamic)
import           Reactive.Commands.Command (Command, ioCommand, performIO)
import           UI.Generic (GenericWidget(..))
import qualified UI.Widget as UIT


newtype Node = Node { unNode :: JSVal } deriving (PToJSVal, PFromJSVal)

instance UIWidget    Node
instance UIContainer Node

instance Focusable Model.Node where
    mayFocus LeftButton _ _ _ = True
    mayFocus _          _ _ _ = False


foreign import javascript unsafe "new GraphNode(-1, new THREE.Vector2($2, $3), 0, $1)" create' :: WidgetId -> Double -> Double -> IO Node
foreign import javascript unsafe "$1.setExpandedStateBool($2)"     setExpandedState :: Node -> Bool     -> IO ()
foreign import javascript unsafe "$1.label($2)"                    setLabel         :: Node -> JSString -> IO ()
foreign import javascript unsafe "$1.setValue($2)"                 setValue         :: Node -> JSString -> IO ()
foreign import javascript unsafe "$1.uniforms.selected.value = $2" setSelected      :: Node -> Int      -> IO ()

foreign import javascript unsafe "$1.htmlContainer"                getHTMLContainer :: Node -> IO Element

createNode :: WidgetId -> Model.Node -> IO Node
createNode id model = do
    node <- create' id (model ^. Model.position . x) (model ^. Model.position . y)
    setLabel node $ lazyTextToJSString $ model ^. Model.expression
    return node

selectedState :: Getter Model.Node Int
selectedState = to selectedState' where
    selectedState' model
        | model ^. Model.isFocused  = 2
        | model ^. Model.isSelected = 1
        | otherwise                 = 0

setSelectedState :: Node -> Model.Node -> IO ()
setSelectedState node model = setSelected node $ model ^. selectedState

updateSelectedState :: WidgetId -> Model.Node -> Command (UIRegistry.State a) ()
updateSelectedState id model = performIO $ do
    uiWidget <- UIR.lookup id
    setSelectedState uiWidget model

unselectNode :: WidgetFile a Model.Node -> Command (UIRegistry.State a) ()
unselectNode file = do
    let widgetId  = file ^. objectId
    model' <- UIRegistry.updateWidgetM widgetId $ Model.isSelected .~ False
    updateSelectedState widgetId model'

instance HandlesKeyPressed Model.Node where
    onKeyPressed char _ file model = (action, toCtxDynamic newModel) where
        newModel = model & Model.isExpanded %~ not
        action   = case char of
            '\r' -> do
                widget <- UIR.lookup $ file ^. objectId
                setExpandedState widget (newModel ^. Model.isExpanded)
            _    -> return ()


ifChanged :: (Eq b) => a -> a -> Lens' a b -> IO () -> IO ()
ifChanged old new get action = if (old ^. get) /= (new ^. get) then action
                                                               else return ()

instance UIDisplayObject Model.Node where
    createUI parentId id model = do
        node   <- createNode id model
        parent <- UIR.lookup parentId :: IO GenericWidget
        UIR.register id node
        UIT.add node parent

    updateUI id old model = do
        node <- UIR.lookup id :: IO Node

        setSelectedState node model
        setExpandedState node (model ^. Model.isExpanded)

        ifChanged old model Model.expression $ do
            setLabel node $ lazyTextToJSString $ model ^. Model.expression

        ifChanged old model Model.value $ do
            setValue node $ lazyTextToJSString $ model ^. Model.value

