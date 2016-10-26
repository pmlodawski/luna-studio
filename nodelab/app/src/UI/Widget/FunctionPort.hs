module UI.Widget.FunctionPort where

import           Utils.PreludePlus
import           Utils.Vector

import           GHCJS.Marshal.Pure         (PFromJSVal (..), PToJSVal (..))
import           GHCJS.Types                (JSVal)
import qualified Style.Function             as Function

import           Object.UITypes
import           Object.Widget
import qualified Object.Widget.FunctionPort as Model

import           UI.Generic                 (whenChanged)
import qualified UI.Generic                 as UI
import qualified UI.Registry                as UI
import           UI.Widget                  (UIWidget)
import qualified UI.Widget                  as Widget


newtype FunctionPort = FunctionPort JSVal deriving (PToJSVal, PFromJSVal)

instance UIWidget FunctionPort

foreign import javascript safe "new FunctionPort($1, $2, $3, $4)" create' :: Int -> Double -> Double -> Bool-> IO FunctionPort
foreign import javascript safe "$1.setHovered($2)"             setHovered :: FunctionPort -> Bool -> IO ()
foreign import javascript safe "$1.setColor($2)"                 setColor :: FunctionPort -> Int -> IO ()


create :: WidgetId -> Model.FunctionPort -> IO FunctionPort
create oid model = do
    widget      <- create' (fromWidgetId oid)
                           Function.portWidth
                           (model ^. Model.size . y)
                           (model ^. Model.inputOutput == Model.Input)
    UI.setWidgetPosition (model ^. widgetPosition) widget
    return widget


instance UIDisplayObject Model.FunctionPort where
    createUI parentId wid model = do
        widget   <- create wid model
        parent   <- UI.lookup parentId :: IO Widget.GenericWidget
        UI.register wid widget
        Widget.add widget parent

    updateUI wid old model  = do
        widget <- UI.lookup wid :: IO FunctionPort
        whenChanged old model Model.hovered $
            setHovered widget $ model ^. Model.hovered
