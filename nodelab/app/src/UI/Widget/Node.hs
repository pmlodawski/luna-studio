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

import           Object.UITypes
import           Object.Widget
import qualified Object.Widget.Node            as Model
import qualified Object.Widget.TextBox         as TextBox
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

foreign import javascript unsafe "new GraphNode(-1, new THREE.Vector2($2, $3), 0, $1)" create'          :: WidgetId -> Double -> Double -> IO Node
foreign import javascript unsafe "$1.setExpandedStateBool($2)"                         setExpandedState :: Node -> Bool     -> IO ()
foreign import javascript unsafe "$1.setZPos($2)"                                      setZPos          :: Node -> Double -> IO ()
foreign import javascript unsafe "$1.uniforms.selected.value = $2"                     setSelected      :: Node -> Int      -> IO ()

createNode :: WidgetId -> Model.Node -> IO Node
createNode id model = do
    node <- create' id (model ^. Model.position . x) (model ^. Model.position . y)
    return node

selectedState :: Getter Model.Node Int
selectedState = to selectedState' where
    selectedState' model
        | model ^. Model.isFocused  = 2
        | model ^. Model.isSelected = 1
        | otherwise                 = 0

setSelectedState :: Node -> Model.Node -> IO ()
setSelectedState node model = setSelected node $ model ^. selectedState

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
