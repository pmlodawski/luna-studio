module Reactive.Plugins.Core.Action.Commands.PendingNode where

import           Utils.PreludePlus
import           Object.Widget.Node (PendingNode(..))
import qualified Object.Widget.Node as WNode
import           Object.Node        (Node, nodePos, expression)
import           Object.Widget      (WidgetFile, objectId, widget)
import           JS.Node            (createPendingNode)
import           Utils.Vector       (Vector2, lengthSquared)

import           Reactive.Plugins.Core.Action.State.Global     (State)
import qualified Reactive.Plugins.Core.Action.State.Global     as Global
import qualified Reactive.Plugins.Core.Action.State.UIRegistry as UIRegistry
import           Reactive.Plugins.Core.Action.Commands.Command (Command, performIO)

import           Reactive.Plugins.Core.Action.Commands.UIRegistry.RemoveWidget (removeWidget)

renderPending :: Node -> Command State ()
renderPending node = do
    let pendingNode = PendingNode (node ^. nodePos) (node ^. expression)
    file <- zoom Global.uiRegistry $ UIRegistry.registerM UIRegistry.sceneGraphId pendingNode def
    performIO $ createPendingNode (file ^. objectId) (node ^. expression) (node ^. nodePos)

getAllPending :: Command (UIRegistry.State a) [WidgetFile a PendingNode]
getAllPending = UIRegistry.lookupAllM

distanceDelta :: Double
distanceDelta = 0.1

areCloseEnough :: Vector2 Double -> Vector2 Double -> Bool
areCloseEnough a b = distanceDelta > (lengthSquared $ a - b)

isRightForNode :: Node -> WidgetFile a PendingNode -> Bool
isRightForNode node pendingFile = sameExpression && closeEnough where
    pending        = pendingFile ^. widget
    sameExpression = node ^. expression == pending ^. WNode.expression
    closeEnough    = areCloseEnough (node ^. nodePos) (pending ^. WNode.position)

unrenderPending :: Node -> Command State ()
unrenderPending node = do
    allPending <- zoom Global.uiRegistry getAllPending
    let toRemove = find (isRightForNode node) allPending
    case toRemove of
        Just pending -> zoom Global.uiRegistry $ removeWidget $ pending ^. objectId
        Nothing      -> return ()

