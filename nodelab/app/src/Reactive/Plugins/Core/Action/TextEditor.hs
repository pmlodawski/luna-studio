module Reactive.Plugins.Core.Action.TextEditor where

import           Utils.PreludePlus

import           Event.Event      (Event(..))
import qualified Event.Batch      as Batch
import qualified Event.TextEditor as TextEditor
import qualified JS.TextEditor    as UI

import qualified Reactive.State.Global           as Global
import           Reactive.Commands.Command       (Command, performIO)
import qualified BatchConnector.Monadic.Commands as BatchCmd
import qualified Empire.API.Graph.CodeUpdate as CodeUpdate
import           Empire.API.Data.GraphLocation (GraphLocation)
import qualified Batch.Workspace as Workspace

isCurrentLocation :: GraphLocation -> Command Global.State Bool
isCurrentLocation location = uses (Global.workspace . Workspace.currentLocation) (== location)


toAction :: Event -> Maybe (Command Global.State ())
toAction (Batch      (Batch.CodeUpdated response)) = Just $ do
    shouldProcess <- isCurrentLocation (response ^. CodeUpdate.location)
    when shouldProcess $
        performIO $ UI.setText $ response ^. CodeUpdate.code

toAction (TextEditor (TextEditor.CodeModified code))  = Just $ zoom Global.workspace $ BatchCmd.setCode code
toAction _                                            = Nothing
