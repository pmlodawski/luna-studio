module UI.Widget.Group where

import           Utils.PreludePlus
import           Utils.Vector

import           Data.JSString.Text            (lazyTextToJSString)
import           GHCJS.Marshal.Pure            (PFromJSVal (..), PToJSVal (..))
import           GHCJS.Types                   (JSString, JSVal)

import           Object.UITypes
import           Object.Widget
import qualified Object.Widget.Group           as Model
import qualified Reactive.State.UIRegistry     as UIRegistry
import qualified Reactive.Commands.UIRegistry as UICmd

import           UI.Generic                    (whenChanged)
import qualified UI.Generic                    as UI
import qualified UI.Registry                   as UI
import           UI.Widget                     (UIWidget (..))
import qualified UI.Widget                     as Widget

newtype Group = Group JSVal deriving (PToJSVal, PFromJSVal)

instance UIWidget Group

foreign import javascript safe "new Group($1, $2, $3)"      create'       :: Int   -> Double -> Double -> IO Group
foreign import javascript safe "$1.setVisible($2)"          setVisible'   :: Group -> Bool -> IO ()
foreign import javascript safe "$1.setBgVisible($2)"        setBgVisible' :: Group -> Bool -> IO ()
foreign import javascript safe "$1.setBgColor($2, $3, $4)"  setBgColor'   :: Group -> Double -> Double -> Double -> IO ()

setBgColor :: Group -> Model.Group -> IO ()
setBgColor group model = case model ^. Model.background of
    Just (r, g, b) -> do
        setBgColor' group r g b
        setBgVisible' group True
    Nothing -> setBgVisible' group False

create :: WidgetId -> Model.Group -> IO Group
create oid model = do
    group      <- create' oid (model ^. Model.size . x) (model ^. Model.size . y)
    setBgColor group model
    UI.setWidgetPosition (model ^. widgetPosition) group
    return group

instance UIDisplayObject Model.Group where
    createUI parentId id model = do
        group    <- create id model
        parent   <- UI.lookup parentId :: IO Widget.GenericWidget
        UI.register id group
        Widget.add group parent

    updateUI id old model = do
        group <- UI.lookup id :: IO Group
        setVisible' group $ model ^. Model.visible
        setBgColor  group model

instance CompositeWidget Model.Group where
    updateWidget id old model = do
        let vis = model ^. Model.visible
        when (old ^. Model.visible /= vis) $ do
            parent <- UICmd.parent id
            UICmd.triggerChildrenResized parent id
