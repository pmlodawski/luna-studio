module Reactive.Commands.EnterNode where

import Utils.PreludePlus
import Reactive.State.Global           (State)
import Reactive.Commands.UnrenderGraph (unrender)
import Reactive.Commands.Command       (Command, performIO)

import           Empire.API.Data.Node (Node)
import qualified Empire.API.Data.Node as Node
import Object.UITypes (WidgetId)

enterNode :: Node -> WidgetId -> Command State ()
enterNode node _ = performIO $ putStrLn "TODO: EnterNode"
    -- if   isDef node
    --                then performEnter node
    --                else performIO $ putStrLn "not entering"
    --
performEnter :: Node -> Command State ()
performEnter node = unrender
