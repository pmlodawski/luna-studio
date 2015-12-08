module UI.Command.Choice where

import           Utils.PreludePlus hiding (Choice)
import           Utils.Vector

import qualified Reactive.State.Global           as Global
import           Reactive.Commands.Command       (Command, performIO)

import           Reactive.State.UIRegistry     (sceneInterfaceId, sceneGraphId, addHandler)
import qualified Reactive.State.Global        as Global
import qualified Reactive.State.UIRegistry    as UIRegistry
import qualified Reactive.Commands.UIRegistry as UICmd

import           Object.Widget.Choice.RadioButton  (RadioButton(..))
import qualified Object.Widget.Choice.RadioButton as RadioButton
import qualified Object.Widget.Group              as Group
import qualified Object.Widget.Label              as Label
import qualified UI.Handlers.Choice.RadioButton   as RadioButton
import           UI.Widget.Choice.RadioButton     ()
import           UI.Command.Group                 as Group

import           Object.Widget.Choice  (Choice(..))
import qualified Object.Widget.Choice  as Choice
import qualified UI.Handlers.Choice    as Choice
import           UI.Widget.Choice ()

import           UI.Layout as Layout
import           UI.Instances
import           Object.UITypes
import           Object.Widget
import           Reactive.State.Global (inRegistry)
import qualified Data.HMap.Lazy as HMap
import           Data.HMap.Lazy (HTMap)
import           Data.HMap.Lazy (TypeKey(..))
import           UI.Handlers.Generic (triggerValueChanged)

radioHandlers :: WidgetId -> Word -> HTMap
radioHandlers id ix = addHandler (RadioButton.SelectedHandler $ selectRadioButton id ix)
                    $ mempty

selectRadioButton :: WidgetId -> Word -> Command Global.State ()
selectRadioButton = setValue

makeChoice :: WidgetId -> Choice -> Command UIRegistry.State WidgetId
makeChoice parent model = do
    contId  <- UICmd.register parent model def
    groupId <- UICmd.register contId Group.create def
    labelId <- UICmd.register contId (Label.create (model ^. Choice.label)) def

    UICmd.moveX groupId 90

    let opts = (model ^. Choice.options) `zip` [0..]
    forM_ opts $ \(label, ix) -> do
        let isSelected = ix == model ^. Choice.value
            widget     = RadioButton def (Vector2 180 20) label isSelected
        UICmd.register_ groupId widget (radioHandlers contId ix)

    Layout.verticalLayout 0.0 groupId
    Group.updateSize contId

    return contId

setValue :: WidgetId -> Word -> Command Global.State ()
setValue id val = do
    inRegistry $ do
        oldVal <- UICmd.get id Choice.value
        items' <- UICmd.children id
        items  <- UICmd.children (head items')
        UICmd.update_ id $ Choice.value .~ val

        let oldWidget = fromMaybe (error "choice#setValue: invalid value") $ items ^? ix (fromIntegral oldVal)
            newWidget = fromMaybe (error "choice#setValue: invalid value") $ items ^? ix (fromIntegral val)

        UICmd.update_ oldWidget $ RadioButton.selected .~ False
        UICmd.update_ newWidget $ RadioButton.selected .~ True

    triggerValueChanged val id
