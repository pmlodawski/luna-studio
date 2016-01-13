module Reactive.Plugins.Core.Action.TextEditor where

import           Utils.PreludePlus

import           Event.Event      (Event(..))
import qualified Event.Batch      as Batch
import qualified Event.TextEditor as TextEditor
import qualified JS.TextEditor    as UI

import qualified Reactive.State.Global           as Global
import           Reactive.Commands.Command       (Command, performIO)
import qualified BatchConnector.Monadic.Commands as BatchCmd
import qualified Empire.API.Graph.GetCode as GetCode
import           Empire.API.Data.GraphLocation (GraphLocation)
import qualified Empire.API.Response as Response
import qualified Batch.Workspace as Workspace

isCurrentLocation :: GraphLocation -> Command Global.State Bool
isCurrentLocation location = uses (Global.workspace . Workspace.currentLocation) (== location)


toAction :: Event -> Maybe (Command Global.State ())
toAction (Batch      (Batch.CodeUpdated response)) = Just $ do
    shouldProcess <- isCurrentLocation (response ^. Response.request . GetCode.location)
    when shouldProcess $
        performIO $ UI.setText $ response ^. Response.update . GetCode.code

toAction (TextEditor (TextEditor.CodeModified code))  = Just $ zoom Global.workspace $ BatchCmd.setCode code
toAction _                                            = Nothing
