module UI.Handlers where

import           Utils.PreludePlus
import           Utils.CtxDynamic
import           Data.Typeable (typeOf)
import           Reactive.State.Global (State)

import           Object.Widget              hiding (setPosition)
import           Object.Widget.Node                (Node, PendingNode)
import           Object.Widget.Connection          (Connection, CurrentConnection)
import           Object.Widget.Port                (Port)
import           Object.Widget.Number.Discrete     (DiscreteNumber)
import           Object.Widget.Number.Continuous   (ContinuousNumber)
import           Object.Widget.Slider.Discrete     (DiscreteSlider)
import           Object.Widget.Slider.Continuous   (ContinuousSlider)
import           Object.Widget.Toggle              (Toggle)
import           Object.Widget.TextBox             (TextBox)
import           Object.Widget.Choice.RadioButton  (RadioButton)
import           Object.Widget.Group               (Group)
import           Object.Widget.Button              (Button)

import           UI.Widget                         (UIWidget, UIContainer, GenericWidget(..))
import qualified UI.Widget.Node                    as Node
import qualified UI.Widget.Slider                  as Slider
import qualified UI.Widget.Connection              as Connection
import qualified UI.Widget.Port                    as Port
import qualified UI.Widget.Toggle                  as Toggle
import qualified UI.Widget.TextBox                 as TextBox
import qualified UI.Widget.Choice                  as Choice
import qualified UI.Widget.Choice.RadioButton      as RadioButton
import qualified UI.Widget.Group                   as Group
import qualified UI.Widget.Button                  as Button

import qualified UI.Handlers.Number.Discrete       as DiscreteNumber
import qualified UI.Handlers.Number.Continuous     as ContinuousNumber
import qualified UI.Handlers.Slider.Discrete       as DiscreteSlider
import qualified UI.Handlers.Slider.Continuous     as ContinuousSlider
import qualified UI.Handlers.Toggle                as Toggle
import qualified UI.Handlers.TextBox               as TextBox
import qualified UI.Handlers.Choice                as Choice
import qualified UI.Handlers.Choice.RadioButton    as RadioButton
import qualified UI.Handlers.Button                as Button

class HasHandlers a where
    widgetHandlers :: DisplayObject -> UIHandlers a

nodeType              = typeOf (undefined :: Node)
connectionType        = typeOf (undefined :: Connection)
currentConnectionType = typeOf (undefined :: CurrentConnection)
pendingNodeType       = typeOf (undefined :: PendingNode)
portType              = typeOf (undefined :: Port)

discreteNumberType    = typeOf (undefined :: DiscreteNumber)
continuousNumberType  = typeOf (undefined :: ContinuousNumber)
discreteSliderType    = typeOf (undefined :: DiscreteSlider)
continuousSliderType  = typeOf (undefined :: ContinuousSlider)
toggleType            = typeOf (undefined :: Toggle)
textBoxType           = typeOf (undefined :: TextBox)
radioButtonType       = typeOf (undefined :: RadioButton)
groupType             = typeOf (undefined :: Group)
buttonType            = typeOf (undefined :: Button)

instance HasHandlers State where
    widgetHandlers (CtxDynamic tpe _)
        | tpe == nodeType              =             Node.widgetHandlers
        | tpe == connectionType        = def -- Connection.widgetHandlers
        | tpe == currentConnectionType = def --CurrentConnection.widgetHandlers
        | tpe == pendingNodeType       = def
        | tpe == portType              = def

        | tpe ==   discreteNumberType  =   DiscreteNumber.widgetHandlers
        | tpe == continuousNumberType  = ContinuousNumber.widgetHandlers
        | tpe ==   discreteSliderType  =   DiscreteSlider.widgetHandlers
        | tpe == continuousSliderType  = ContinuousSlider.widgetHandlers
        | tpe ==           toggleType  =           Toggle.widgetHandlers
        | tpe ==          textBoxType  =          TextBox.widgetHandlers
        | tpe ==      radioButtonType  =      RadioButton.widgetHandlers
        | tpe ==            groupType  = def
        | tpe ==           buttonType  =           Button.widgetHandlers

        | otherwise                    = error $ "Unknown widget type " <> (show tpe)
