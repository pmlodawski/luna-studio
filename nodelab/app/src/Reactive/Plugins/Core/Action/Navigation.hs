module Reactive.Plugins.Core.Action.Navigation where


import           Utils.PreludePlus

-- import qualified Data.Set                          as Set

import           Object.Widget                     (WidgetFile, WidgetId, objectId, widget)
import qualified Object.Widget.Node                as NodeModel

import           Event.Event                       (Event (Mouse, Keyboard), JSState)
import           Event.Keyboard                    (KeyMods (..), shift)
import qualified Event.Keyboard                    as Keyboard
import qualified Event.Mouse                       as Mouse

import           Reactive.State.Global             (State, inRegistry)
-- import qualified Reactive.State.Global             as Global
import qualified Reactive.State.UIRegistry         as UIRegistry

import qualified Empire.API.Data.Node           as N

import           Reactive.Commands.Command         (Command, performIO)
import           Reactive.Commands.Graph           (allNodes)
import           Reactive.Commands.Graph.Selection (focusSelectedNode, selectAll, selectedNodes, unselectAll)
-- import qualified Reactive.Commands.UIRegistry      as UICmd


toAction :: Event -> Maybe (Command State ())
toAction (Keyboard _ (Keyboard.Event Keyboard.Down char KeyMods { _shift = True })) = case char of
    '\37' -> Just goLeft
    '\39' -> Just goRight
    '\38' -> Just goUp
    '\40' -> Just goDown
    _     -> Nothing
toAction (Keyboard _ (Keyboard.Event Keyboard.Down char (KeyMods False False False False))) = case char of
    '\37' -> Just goLeft
    '\39' -> Just goRight
    '\38' -> Just goUp
    '\40' -> Just goDown
    _     -> Nothing
toAction (Keyboard _ (Keyboard.Event Keyboard.Down '\t' KeyMods { _shift = True })) = Just goPrev
toAction _ = Nothing

goLeft, goRight, goUp, goDown :: Command State ()
goLeft  = test
-- goRight = test
goUp    = test
goDown  = test

goPrev, goNext :: Command State ()
goPrev  = test
goNext  = test

test :: Command State ()
test = do
    focusedWidget <- inRegistry $ use UIRegistry.focusedWidget
    when (isNothing focusedWidget) selectAll


goRight = do
    -- getSelectedNode
    -- getItsParams
    -- findNext
    -- selectIt
    widgets <- allNodes
    nodesToMove <- inRegistry $ forM widgets $ \wf -> do
        let widgetId = wf ^. objectId
        if (wf ^. widget . NodeModel.isSelected) then do
                -- UICmd.update_ widgetId $ NodeModel.isSelected .~ False
                return $ Just $ wf ^. widget . NodeModel.nodeId
        else return Nothing

    mapM moveRight $ listToMaybe $ catMaybes nodesToMove
    return ()

moveRight :: N.NodeId -> Command State ()
moveRight nodeId = do
    return ()
