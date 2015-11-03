module Reactive.State.UnderCursor where


import           Utils.PreludePlus
import           Utils.Vector

import           Object.Object
import           Object.Port
import           Object.Node
import qualified Object.Widget.Node as Model
import qualified Object.Widget.Port as Model

import qualified Reactive.State.Global     as Global
import qualified Reactive.State.Graph      as Graph
import qualified Reactive.State.UIRegistry as UIRegistry

import           Object.Widget
import           Object.UITypes
import           Utils.CtxDynamic
import           Reactive.Commands.Command    (Command, performIO, execCommand)


isNodeUnderCursor :: Command (UIRegistry.State Global.State) Bool
isNodeUnderCursor = do
    maybeOver    <- use UIRegistry.widgetOver
    case maybeOver of
        Just widgetId -> do
            widget <- UIRegistry.lookupTypedM widgetId :: Command (UIRegistry.State b) (Maybe (WidgetFile b Model.Node))
            return $ isJust widget
        Nothing       -> return False

getPortRefUnderCursor :: Global.State -> Maybe PortRef
getPortRefUnderCursor state = do
    let registry = state ^. Global.uiRegistry
    widgetId    <- registry ^. UIRegistry.widgetOver
    maybeWidget <- UIRegistry.lookup widgetId registry
    widget      <- (fromCtxDynamic (maybeWidget ^. widget)) :: Maybe Model.Port
    return $ widget ^. Model.portRef
