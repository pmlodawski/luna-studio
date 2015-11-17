module UI.Handlers where

import           Utils.PreludePlus
import           Utils.CtxDynamic
import           Data.Typeable (typeOf)

import           Object.Widget hiding (setPosition)
import           Object.Widget.Node (Node, PendingNode)
import           Object.Widget.Connection (Connection, CurrentConnection)
import           Object.Widget.Port (Port)
import           Object.Widget.Number.Discrete   (DiscreteNumber)
import           Object.Widget.Number.Continuous (ContinuousNumber)
import           Object.Widget.Slider.Discrete   (DiscreteSlider)
import           Object.Widget.Slider.Continuous (ContinuousSlider)
import           Reactive.State.Global (State)

import           UI.Widget (UIWidget, UIContainer, GenericWidget(..))
import qualified UI.Widget.Node as Node
import qualified UI.Widget.Slider as Slider
import qualified UI.Widget.Connection as Connection
import qualified UI.Widget.Port as Port
import qualified UI.Handlers.Number.Discrete   as DiscreteNumber
import qualified UI.Handlers.Number.Continuous as ContinuousNumber
import qualified UI.Handlers.Slider.Discrete   as DiscreteSlider
import qualified UI.Handlers.Slider.Continuous as ContinuousSlider


class HasHandlers a where
    widgetHandlers :: DisplayObject -> UIHandlers a

nodeType              = typeOf (undefined :: Node)
discreteNumberType    = typeOf (undefined :: DiscreteNumber)
continuousNumberType  = typeOf (undefined :: ContinuousNumber)
discreteSliderType    = typeOf (undefined :: DiscreteSlider)
continuousSliderType  = typeOf (undefined :: ContinuousSlider)
connectionType        = typeOf (undefined :: Connection)
currentConnectionType = typeOf (undefined :: CurrentConnection)
pendingNodeType       = typeOf (undefined :: PendingNode)
portType              = typeOf (undefined :: Port)

instance HasHandlers State where
    widgetHandlers (CtxDynamic tpe _)
        | tpe == nodeType                =             Node.widgetHandlers
        | tpe ==   discreteSliderType    =   DiscreteSlider.widgetHandlers
        | tpe == continuousSliderType    = ContinuousSlider.widgetHandlers
        | tpe ==   discreteNumberType    =   DiscreteNumber.widgetHandlers
        | tpe == continuousNumberType    = ContinuousNumber.widgetHandlers
        | tpe == connectionType          = def -- Connection.widgetHandlers
        | tpe == currentConnectionType   = def --CurrentConnection.widgetHandlers
        | tpe == pendingNodeType         = def
        | tpe == portType                = def
        | otherwise                      = error $ "Unknown widget type " <> (show tpe)
