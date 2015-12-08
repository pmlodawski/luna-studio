module UI.Command.Slider.Discrete where

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
import           Object.Widget.Slider.Discrete   as DiscreteSlider hiding (setValue)
import qualified UI.Handlers.Slider.Discrete     as DiscreteSlider
import qualified Object.Widget.TextBox           as TextBox
import qualified UI.Handlers.TextBox             as TextBox

import           Object.UITypes
import           Object.Widget
import           Reactive.State.Global (inRegistry)

import qualified Data.HMap.Lazy as HMap
import           Data.HMap.Lazy (HTMap)
import           Data.HMap.Lazy (TypeKey(..))

import           Data.Text.Lazy.Read (decimal)
import qualified Data.Text.Lazy as Text
import           UI.Handlers.Generic (triggerValueChanged, ValueChangedHandler(..))

textHandlers :: WidgetId -> HTMap
textHandlers id = addHandler (ValueChangedHandler $ textValueChangedHandler id)
                $ mempty where

textValueChangedHandler :: WidgetId -> Text -> WidgetId -> Command Global.State ()
textValueChangedHandler parent val tbId = do
    let val' = decimal val
    case val' of
        Left err        -> inRegistry $ do
            val <- UICmd.get parent DiscreteSlider.value
            DiscreteSlider.setValue parent val
        Right (val', _) -> do
            inRegistry $ DiscreteSlider.setValue parent val'
            triggerValueChanged val' parent

makeDiscreteSlider :: WidgetId -> DiscreteSlider -> HTMap -> Command UIRegistry.State WidgetId
makeDiscreteSlider parent model handlers = do
    widgetId <- UICmd.register parent model handlers

    let tx      = (model ^. size . x) / 2.0
        ty      = (model ^. size . y)
        sx      = tx - (model ^. size . y / 2.0)
        textVal = Text.pack $ show $ model ^. value
        textBox = TextBox.create (Vector2 sx ty) textVal TextBox.Right

    tbId <- UICmd.register widgetId textBox $ textHandlers widgetId
    UICmd.moveX tbId tx

    return widgetId


