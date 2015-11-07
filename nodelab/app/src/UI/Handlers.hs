{-# LANGUAGE JavaScriptFFI #-}
{-# LANGUAGE OverloadedStrings #-}

module UI.Handlers where

import           Utils.PreludePlus
import           Utils.CtxDynamic
import           Data.Typeable (typeOf)

import           Object.Widget hiding (setPosition)
import           Object.Widget.Node (Node, PendingNode)
import           Object.Widget.Slider (Slider)
import           Object.Widget.Connection (Connection, CurrentConnection)
import           Object.Widget.Port (Port)
import           Reactive.State.Global (State)

import           UI.Widget (UIWidget, UIContainer, GenericWidget(..))
import qualified UI.Widget.Node as Node
import qualified UI.Widget.Slider as Slider
import qualified UI.Widget.Connection as Connection
import qualified UI.Widget.Port as Port

class HasHandlers a where
    widgetHandlers :: DisplayObject -> UIHandlers a

nodeType              = typeOf (undefined :: Node)
sliderDoubleType      = typeOf (undefined :: Slider Double)
sliderIntType         = typeOf (undefined :: Slider Int)
connectionType        = typeOf (undefined :: Connection)
currentConnectionType = typeOf (undefined :: CurrentConnection)
pendingNodeType       = typeOf (undefined :: PendingNode)
portType              = typeOf (undefined :: Port)

instance HasHandlers State where
    widgetHandlers (CtxDynamic tpe _)
        | tpe == nodeType                = Node.widgetHandlers
        | tpe == sliderDoubleType        = Slider.widgetHandlers
        -- | tpe == sliderIntType           = def --Slider.widgetHandlers
        | tpe == connectionType          = def -- Connection.widgetHandlers
        | tpe == currentConnectionType   = def --CurrentConnection.widgetHandlers
        | tpe == pendingNodeType         = def
        | tpe == portType                = def
        | otherwise                      = error $ "Unknown widget type " <> (show tpe)
