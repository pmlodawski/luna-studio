module UI.Handlers.Choice where

import           Utils.PreludePlus                hiding (Choice)

import           Data.HMap.Lazy                   (HTMap, TypeKey (..))
import           Utils.Vector

import qualified Event.Mouse                      as Mouse
import           Object.Widget                    (UIHandlers, WidgetId, CompositeWidget, createWidget, updateWidget, ResizableWidget, resizeWidget)
import           Object.Widget.Choice             (Choice (..))
import           Object.Widget.Choice.RadioButton (RadioButton (..))

import           Reactive.Commands.Command        (Command)
import           Reactive.State.Global            (inRegistry)
import           Reactive.State.UIRegistry        (addHandler)
import qualified Reactive.State.UIRegistry        as UIRegistry

import           UI.Command.Group                 as Group
import           UI.Generic                       (startDrag, defaultResize)
import           UI.Handlers.Generic              (triggerValueChanged)
import           UI.Handlers.Group                ()
import           UI.Layout                        as Layout

import           UI.Widget.Choice                 ()
import           UI.Widget.Choice.RadioButton     ()
import           UI.Widget.Group                  ()
import           UI.Widget.Label                  ()
import           UI.Widget.Toggle                 ()

import qualified Data.HMap.Lazy                   as HMap

import qualified Object.Widget.Choice             as Choice
import qualified Object.Widget.Choice.RadioButton as RadioButton
import qualified Object.Widget.Group              as Group
import qualified Object.Widget.Label              as Label
import qualified Reactive.Commands.UIRegistry     as UICmd
import qualified Reactive.State.Global            as Global
import qualified UI.Handlers.Choice.RadioButton   as RadioButton


widgetHandlers :: UIHandlers Global.State
widgetHandlers = def


-- Constructors --

radioHandlers :: WidgetId -> Word -> HTMap
radioHandlers id ix = addHandler (RadioButton.SelectedHandler $ selectRadioButton id ix)
                    $ mempty

selectRadioButton :: WidgetId -> Word -> Command Global.State ()
selectRadioButton id val = inRegistry $ UICmd.update_ id $ Choice.value .~ val

instance CompositeWidget Choice where
    createWidget id model = do
        groupId <- UICmd.register id Group.create (Layout.verticalLayoutHandler def 0.0)
        labelId <- UICmd.register id (Label.create (Vector2 100.0 20.0)(model ^. Choice.label)) def

        UICmd.moveX groupId 90

        let opts = (model ^. Choice.options) `zip` [0..]
        forM_ opts $ \(label, ix) -> do
            let isSelected = ix == model ^. Choice.value
                widget     = RadioButton def (Vector2 180 20) label isSelected
            UICmd.register_ groupId widget (radioHandlers id ix)

        Layout.verticalLayout def 0.0 groupId
        Group.updateSize def id

    updateWidget id old model = do
        let val    = model ^. Choice.value
            oldVal = old   ^. Choice.value
        items' <- UICmd.children id
        items  <- UICmd.children (head items')

        let oldWidget = fromMaybe (error "choice#setValue: invalid value") $ items ^? ix (fromIntegral oldVal)
            newWidget = fromMaybe (error "choice#setValue: invalid value") $ items ^? ix (fromIntegral val)

        UICmd.update_ oldWidget $ RadioButton.selected .~ False
        UICmd.update_ newWidget $ RadioButton.selected .~ True

instance ResizableWidget Choice where resizeWidget = defaultResize
