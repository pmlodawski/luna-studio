module UI.Widget.DefinitionPort where

import           Utils.PreludePlus
import           Utils.Vector

import           Data.JSString.Text           (lazyTextToJSString)
import           GHCJS.Marshal.Pure           (PFromJSVal (..), PToJSVal (..))
import           GHCJS.Types                  (JSString, JSVal)
import qualified Style.Definition             as Definition

import           Object.UITypes
import           Object.Widget
import qualified Object.Widget.DefinitionPort as Model

import           UI.Generic                   (whenChanged)
import qualified UI.Generic                   as UI
import qualified UI.Registry                  as UI
import           UI.Widget                    (UIWidget)
import qualified UI.Widget                    as Widget


newtype DefinitionPort = DefinitionPort JSVal deriving (PToJSVal, PFromJSVal)

instance UIWidget DefinitionPort

foreign import javascript safe "new DefinitionPort($1, $2, $3, $4)" create' :: Int -> Double -> Double -> Bool-> IO DefinitionPort
foreign import javascript safe "$1.setHovered($2)"               setHovered :: DefinitionPort -> Bool -> IO ()
foreign import javascript safe "$1.setColor($2)"                   setColor :: DefinitionPort -> Int -> IO ()


create :: WidgetId -> Model.DefinitionPort -> IO DefinitionPort
create oid model = do
    widget      <- create' (fromWidgetId oid)
                           Definition.portWidth
                           (model ^. Model.size . y)
                           (model ^. Model.inputOutput == Model.Input)
    UI.setWidgetPosition (model ^. widgetPosition) widget
    return widget


instance UIDisplayObject Model.DefinitionPort where
    createUI parentId wid model = do
        widget   <- create wid model
        parent   <- UI.lookup parentId :: IO Widget.GenericWidget
        UI.register wid widget
        Widget.add widget parent

    updateUI wid old model  = do
        widget <- UI.lookup wid :: IO DefinitionPort
        whenChanged old model Model.hovered $
            setHovered widget $ model ^. Model.hovered
