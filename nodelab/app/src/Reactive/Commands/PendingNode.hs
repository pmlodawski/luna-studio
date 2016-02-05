module Reactive.Commands.PendingNode where

import           Utils.PreludePlus
import           Object.Widget.Node (PendingNode(..))
import qualified Object.Widget.Node as Model
import           Empire.API.Data.Node (Node)
import qualified Empire.API.Data.Node as Node
import           Object.Widget      (WidgetFile, objectId, widget, UIDisplayObject(..), CompositeWidget, createWidget, updateWidget, ResizableWidget)
import           JS.Node            (createPendingNode)
import           Utils.Vector       (Vector2, lengthSquared, fromTuple)

import           Reactive.State.Global     (State)
import qualified Reactive.State.Global     as Global
import qualified Reactive.State.UIRegistry as UIRegistry
import           Reactive.Commands.Command (Command, performIO)

import           Reactive.Commands.UIRegistry (removeWidget)
import           Empire.API.Data.Node (Node)
import qualified Empire.API.Data.Node as Node

-- renderPending :: Node -> Command State ()
-- renderPending node = do
--     let pendingNode = PendingNode (node ^. Node.expression) (fromTuple $ node ^. Node.position)
--     file <- zoom Global.uiRegistry $ UIRegistry.registerM UIRegistry.sceneGraphId pendingNode def
--     performIO $ createPendingNode (file ^. objectId) (node ^. Node.expression) (fromTuple $ node ^. Node.position)
--
--     -- TODO: FIXME: use tag in request to mark which is which
--
-- getAllPending :: Command UIRegistry.State [WidgetFile PendingNode]
-- getAllPending = UIRegistry.lookupAllM
--
-- distanceDelta :: Double
-- distanceDelta = 0.1
--
-- areCloseEnough :: Vector2 Double -> Vector2 Double -> Bool
-- areCloseEnough a b = distanceDelta > (lengthSquared $ a - b)
--
-- isRightForNode :: Node -> WidgetFile PendingNode -> Bool
-- isRightForNode node pendingFile = sameExpression && closeEnough where
--     pending        = pendingFile ^. widget
--     sameExpression = node ^. Node.expression == pending ^. Model.pendingExpression
--     closeEnough    = areCloseEnough (fromTuple $ node ^. Node.position) (pending ^. Model.pendingPosition)
--
-- unrenderPending :: Node -> Command State ()
-- unrenderPending node = do
--     allPending <- zoom Global.uiRegistry getAllPending
--     let toRemove = find (isRightForNode node) allPending
--     case toRemove of
--         Just pending -> zoom Global.uiRegistry $ removeWidget $ pending ^. objectId
--         Nothing      -> return ()
--

instance UIDisplayObject PendingNode where
    createUI = undefined
    updateUI = undefined

instance CompositeWidget PendingNode where
    createWidget _   _ = return ()
    updateWidget _ _ _ = return ()

instance ResizableWidget PendingNode
