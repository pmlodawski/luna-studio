module UI.Command.Number where

import           Utils.PreludePlus
import           Utils.Vector
import           Control.Monad (forM, foldM)

import           Object.UITypes
import           Object.Widget
import qualified Reactive.State.Global           as Global
import           Reactive.Commands.Command       (Command, performIO)

import           Reactive.State.UIRegistry     (sceneInterfaceId, sceneGraphId, addHandler)
import qualified Reactive.State.UIRegistry     as UIRegistry
import qualified Reactive.Commands.UIRegistry  as UICmd

import           UI.Instances
import           Object.Widget.Number.Discrete as DiscreteNumber
import qualified UI.Handlers.Number.Discrete   as DiscreteNumber
import qualified Object.Widget.TextBox         as TextBox
import qualified UI.Handlers.TextBox           as TextBox

import           Object.UITypes
import           Object.Widget
import           Reactive.State.Global (inRegistry)

import qualified Data.HMap.Lazy as HMap
import           Data.HMap.Lazy (HTMap)
import           Data.HMap.Lazy (TypeKey(..))

import           Data.Text.Lazy.Read (decimal)
import qualified Data.Text.Lazy as Text

textHandlers :: WidgetId -> HTMap
textHandlers id = addHandler (TextBox.ValueChangedHandler $ textValueChangedHandler id)
                $ mempty where

textValueChangedHandler :: WidgetId -> Text -> WidgetId -> Command Global.State ()
textValueChangedHandler parent val tbId = do
    let val' = decimal val
    case val' of
        Left err        -> inRegistry $ do
            val <- UICmd.get parent DiscreteNumber.value
            DiscreteNumber.setValue parent val
        Right (val', _) -> do
            inRegistry $ DiscreteNumber.setValue parent val'
            DiscreteNumber.triggerValueChanged val' parent

makeDiscreteNumber :: WidgetId -> DiscreteNumber -> HTMap -> Command UIRegistry.State WidgetId
makeDiscreteNumber parent model handlers = do
    widgetId <- UICmd.register parent model handlers

    let tx      = (model ^. size . x) / 2.0
        ty      = (model ^. size . y)
        textVal = Text.pack $ show $ model ^. value
        textBox = TextBox.create (Vector2 tx ty) $ textVal

    tbId <- UICmd.register widgetId textBox $ textHandlers widgetId
    UICmd.moveX tbId tx

    return widgetId


