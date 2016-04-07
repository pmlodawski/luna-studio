{-# LANGUAGE OverloadedStrings #-}
module Reactive.Commands.NodeSearcher where


import           Data.Map                                        (Map)
import qualified Data.Map                                        as Map
import           Utils.PreludePlus
import           Utils.Vector

import qualified JS.NodeSearcher                                 as UI

import qualified Batch.Workspace                                 as Workspace
import qualified Event.Keyboard                                  as Keyboard
import qualified Event.NodeSearcher                              as NodeSearcher
import qualified Object.Node                                     as Node
import qualified Object.Widget                                   as Widget
import qualified Object.Widget.Node                              as NodeModel

import           Reactive.Commands.Command                       (Command, performIO)
import           Reactive.Commands.RegisterNode                  (registerNode)
import           Reactive.Commands.UpdateNode                    ()
import qualified Reactive.State.Global                           as Global
import qualified Reactive.State.Graph                            as Graph
import qualified Reactive.State.UIRegistry                       as UIRegistry

import           Empire.API.Data.NodeSearcher                    (Item (..), LunaModule (..))
import qualified Reactive.Plugins.Core.Action.NodeSearcher.Scope as Scope

searcherData :: Command Global.State LunaModule
searcherData = use $ Global.workspace . Workspace.nodeSearcherData

openFresh :: Command Global.State ()
openFresh = do
    mousePos <- use Global.mousePos
    performIO $ UI.initNodeSearcher "" 0 mousePos False

querySearch :: Text -> Command Global.State ()
querySearch query = do
    sd <- searcherData
    let items = Scope.searchInScope sd query
    performIO $ UI.displayQueryResults items

queryTree :: Text -> Command Global.State ()
queryTree query = do
    sd <- searcherData
    let items = Scope.moduleItems sd query
    performIO $ UI.displayTreeResults items

openCommand :: Command Global.State ()
openCommand = do
    mousePos <- use Global.mousePos
    performIO $ UI.initNodeSearcher "" 0 mousePos True
