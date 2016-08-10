module Reactive.Plugins.Core.Action.Navigation where


import           Data.List (maximumBy)
import           Data.Function (on)

import           Utils.PreludePlus
import           Utils.Vector

import           Object.Widget                     (WidgetFile, WidgetId, objectId, widget, Position)
import qualified Object.Widget.Node                as Model

import           Event.Event                       (Event (Mouse, Keyboard), JSState)
import           Event.Keyboard                    (KeyMods (..), shift)
import qualified Event.Keyboard                    as Keyboard
import qualified Event.Mouse                       as Mouse

import           Reactive.State.Global             (State, inRegistry)
import qualified Reactive.State.UIRegistry         as UIRegistry

import qualified Empire.API.Data.Node              as N

import           Reactive.Commands.Command         (Command, performIO)
import           Reactive.Commands.Graph           (allNodes)
import           Reactive.Commands.Graph.Selection (focusSelectedNode, selectAll, selectedNodes, unselectAll)
import qualified Reactive.Commands.UIRegistry      as UICmd



toAction :: Event -> Maybe (Command State ())
toAction (Keyboard _ (Keyboard.Event Keyboard.Down char KeyMods { _shift = True })) = case char of
    '\t'  -> Just goLeft
    '\37' -> Just goLeft
    '\39' -> Just goRight
    '\40' -> Just goDown
    '\38' -> Just goUp
    _     -> Nothing
toAction (Keyboard _ (Keyboard.Event Keyboard.Down char (KeyMods False False False False))) = case char of
    '\37' -> Just goLeft
    '\39' -> Just goRight
    '\40' -> Just goDown
    '\38' -> Just goUp
    _     -> Nothing
toAction _ = Nothing

goPrev, goNext :: Command State ()
goPrev  = return ()
goNext  = return ()

goLeft, goRight, goDown, goUp :: Command State ()
goRight = go findRightMost findNodesOnRight
goLeft  = go findLeftMost  findNodesOnLeft
goDown  = go findDownMost  findNodesOnDown
goUp    = go findUpMost    findNodesOnUp

go :: ([WidgetFile Model.Node] -> WidgetFile Model.Node) -> (Position -> [WidgetFile Model.Node] -> [WidgetFile Model.Node]) -> Command State ()
go findMost findNodesOnSide = do
    nodes <- allNodes
    let selectedNodes = findSelected nodes
    when (not $ null selectedNodes) $ do
        let nodeSrc = findMost selectedNodes
            pos = nodeSrc ^. widget . Model.position
            nodesSide = findNodesOnSide pos nodes
        when (not $ null nodesSide) $ do
            let nodeDst = findNearestNode pos nodesSide
            changeSelection selectedNodes nodeDst

findRightMost, findLeftMost, findDownMost, findUpMost :: [WidgetFile Model.Node] -> WidgetFile Model.Node
findRightMost = maximumBy (compare `on` (^. widget . Model.position . x))
findLeftMost  = minimumBy (compare `on` (^. widget . Model.position . x))
findDownMost  = maximumBy (compare `on` (^. widget . Model.position . y))
findUpMost    = minimumBy (compare `on` (^. widget . Model.position . y))

findNodesOnRight, findNodesOnLeft, findNodesOnDown, findNodesOnUp :: Position -> [WidgetFile Model.Node] -> [WidgetFile Model.Node]
findNodesOnRight = filter . isOnRight
findNodesOnLeft  = filter . isOnLeft
findNodesOnDown  = filter . isOnDown
findNodesOnUp    = filter . isOnUp

isOnRight, isOnLeft, isOnDown, isOnUp :: Position -> WidgetFile Model.Node -> Bool
isOnRight = isOnSide (>) skip (>=)
isOnLeft  = isOnSide (<) skip (>=)
isOnDown  = isOnSide skip (>) (<)
isOnUp    = isOnSide skip (<) (<)

skip :: Double -> Double -> Bool
skip _ _ = True

isOnSide :: (Double -> Double -> Bool) -> (Double -> Double -> Bool) -> (Double -> Double -> Bool) -> Position -> WidgetFile Model.Node -> Bool
isOnSide cmpDXZero cmpDYZero cmpDims pos wf = dx `cmpDXZero` 0.0 && dy `cmpDYZero` 0.0 && abs dx `cmpDims` abs dy where
    nodePos = wf ^. widget . Model.position
    dx = nodePos ^. x - pos ^. x
    dy = nodePos ^. y - pos ^. y

findSelected :: [WidgetFile Model.Node] -> [WidgetFile Model.Node]
findSelected = filter $ \wf -> wf ^. widget . Model.isSelected

findNearestNode :: Position -> [WidgetFile Model.Node] -> WidgetFile Model.Node
findNearestNode pos = minimumBy (compare `on` (distance pos))

distance :: Position -> WidgetFile Model.Node -> Double
distance pos wf = lengthSquared (wpos - pos) where
    wpos = wf ^. widget . Model.position

changeSelection :: [WidgetFile Model.Node] -> WidgetFile Model.Node -> Command State ()
changeSelection selectedNodes nodeDst = inRegistry $ do
    forM selectedNodes $ \node -> UICmd.update_ (node ^. objectId) $ Model.isSelected .~ False
    UICmd.update_ (nodeDst ^. objectId) $ Model.isSelected .~ True
