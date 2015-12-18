{-# LANGUAGE Rank2Types #-}

module UI.Widget.Node where

import           Utils.PreludePlus
import           Utils.Vector
import           Utils.CtxDynamic              (toCtxDynamic)
import           Data.HMap.Lazy                (TypeKey (..))
import qualified Data.HMap.Lazy                as HMap

import           Data.JSString.Text            (lazyTextFromJSString, lazyTextToJSString)
import           GHCJS.DOM.Element             (Element)
import           GHCJS.Marshal.Pure            (PFromJSVal (..), PToJSVal (..))
import           GHCJS.Types                   (JSString, JSVal)

import           Event.Keyboard                (KeyMods (..))
import           Event.Mouse                   (MouseButton (..))
import qualified Event.Mouse                   as Mouse
import           Object.UITypes
import           Object.Widget
import qualified Object.Widget.Node            as Model
import           Reactive.Commands.Command     (Command, ioCommand, performIO)
import qualified Reactive.Commands.UIRegistry  as UICmd
import           Reactive.State.Global         (inRegistry)
import qualified Reactive.State.Global         as Global
import qualified Reactive.State.UIRegistry     as UIRegistry

import           UI.Generic                    (takeFocus)
import qualified UI.Registry                   as UIR
import           UI.Widget                     (UIContainer (..), UIWidget (..))
import           UI.Widget                     (GenericWidget (..))
import qualified UI.Widget                     as UIT

newtype Node = Node { unNode :: JSVal } deriving (PToJSVal, PFromJSVal)

instance UIWidget    Node
instance UIContainer Node

foreign import javascript unsafe "new GraphNode(-1, new THREE.Vector2($2, $3), 0, $1)" create' :: WidgetId -> Double -> Double -> IO Node
foreign import javascript unsafe "$1.setExpandedStateBool($2)"     setExpandedState :: Node -> Bool     -> IO ()
foreign import javascript unsafe "$1.setLabel($2)"                 setLabel         :: Node -> JSString -> IO ()
foreign import javascript unsafe "$1.setValue($2)"                 setValue         :: Node -> JSString -> IO ()
foreign import javascript unsafe "$1.setZPos($2)"                  setZPos          :: Node -> Double -> IO ()
foreign import javascript unsafe "$1.uniforms.selected.value = $2" setSelected      :: Node -> Int      -> IO ()

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

unselectNode :: WidgetId -> Command UIRegistry.State ()
unselectNode = flip UICmd.update_ (Model.isSelected .~ False)

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
        setZPos          node (model ^. Model.zPos)
        setExpandedState node (model ^. Model.isExpanded)

        ifChanged old model Model.expression $ do
            setLabel node $ lazyTextToJSString $ model ^. Model.expression

        ifChanged old model Model.value $ do
            setValue node $ lazyTextToJSString $ model ^. Model.value


newtype RemoveNodeHandler = RemoveNodeHandler (Command Global.State ())
removeNodeHandler = TypeKey :: TypeKey RemoveNodeHandler

newtype FocusNodeHandler = FocusNodeHandler (WidgetId -> Command Global.State ())
focusNodeHandler = TypeKey :: TypeKey FocusNodeHandler

triggerRemoveHandler :: WidgetId -> Command Global.State ()
triggerRemoveHandler id = do
    maybeHandler <- inRegistry $ UICmd.handler id removeNodeHandler
    forM_ maybeHandler $ \(RemoveNodeHandler handler) -> handler

triggerFocusNodeHandler :: WidgetId -> Command Global.State ()
triggerFocusNodeHandler id = do
    maybeHandler <- inRegistry $ UICmd.handler id focusNodeHandler
    forM_ maybeHandler $ \(FocusNodeHandler handler) -> handler id

keyDownHandler :: KeyPressedHandler Global.State
keyDownHandler '\r'   _ _ id = zoom Global.uiRegistry $ UICmd.update_ id (Model.isExpanded %~ not)
keyDownHandler '\x08' _ _ id = triggerRemoveHandler id
keyDownHandler '\x2e' _ _ id = triggerRemoveHandler id
keyDownHandler _      _ _ _  = return ()

handleSelection :: Mouse.Event' -> WidgetId -> Command Global.State ()
handleSelection evt id = case evt ^. Mouse.keyMods of
    KeyMods False False False False -> zoom Global.uiRegistry $ performSelect id
    KeyMods False False True  False -> zoom Global.uiRegistry $ toggleSelect  id
    otherwise                       -> return ()

performSelect :: WidgetId -> Command UIRegistry.State ()
performSelect id = do
    isSelected <- UICmd.get id Model.isSelected
    unless isSelected $ do
        unselectAll
        UICmd.update_ id (Model.isSelected .~ True)

toggleSelect :: WidgetId -> Command UIRegistry.State ()
toggleSelect id = UICmd.update_ id (Model.isSelected %~ not)

unselectAll :: Command UIRegistry.State ()
unselectAll = do
    widgets <- allNodes
    let widgetIds = (^. objectId) <$> widgets
    forM_ widgetIds $ (flip UICmd.update) (Model.isSelected .~ False)

widgetHandlers :: UIHandlers Global.State
widgetHandlers = def & keyDown      .~ keyDownHandler
                     & mousePressed .~ (\evt _ id -> do
                         triggerFocusNodeHandler id
                         takeFocus evt id
                         handleSelection evt id)

allNodes :: Command UIRegistry.State [WidgetFile Model.Node]
allNodes = UIRegistry.lookupAllM

instance CompositeWidget Model.Node
