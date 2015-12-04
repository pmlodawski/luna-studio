module UI.Command.Slider.Continuous where

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
import           Object.Widget.Slider.Continuous as ContinuousSlider hiding (setValue)
import qualified UI.Handlers.Slider.Continuous   as ContinuousSlider
import qualified Object.Widget.TextBox           as TextBox
import qualified UI.Handlers.TextBox             as TextBox

import           Object.UITypes
import           Object.Widget
import           Reactive.State.Global (inRegistry)

import qualified Data.HMap.Lazy as HMap
import           Data.HMap.Lazy (HTMap)
import           Data.HMap.Lazy (TypeKey(..))

import           Data.Text.Lazy.Read (rational)
import qualified Data.Text.Lazy as Text

textHandlers :: WidgetId -> HTMap
textHandlers id = addHandler (TextBox.ValueChangedHandler $ textValueChangedHandler id)
                $ mempty where

textValueChangedHandler :: WidgetId -> Text -> WidgetId -> Command Global.State ()
textValueChangedHandler parent val tbId = do
    let val' = rational val
    case val' of
        Left err        -> inRegistry $ do
            val <- UICmd.get parent ContinuousSlider.value
            ContinuousSlider.setValue parent val
        Right (val', _) -> do
            inRegistry $ ContinuousSlider.setValue parent val'
            ContinuousSlider.triggerValueChanged val' parent

makeContinuousSlider :: WidgetId -> ContinuousSlider -> HTMap -> Command UIRegistry.State WidgetId
makeContinuousSlider parent model handlers = do
    widgetId <- UICmd.register parent model handlers

    let tx      = (model ^. size . x) / 2.0
        ty      = (model ^. size . y)
        sx      = tx - (model ^. size . y / 2.0)
        textVal = Text.pack $ show $ model ^. value
        textBox = TextBox.create (Vector2 sx ty) textVal TextBox.Right

    tbId <- UICmd.register widgetId textBox $ textHandlers widgetId
    UICmd.moveX tbId tx

    return widgetId


