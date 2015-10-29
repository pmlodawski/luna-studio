module Reactive.Plugins.Core.Action.TextEditor where

import           Utils.PreludePlus

import           Event.Event      (Event(..))
import qualified Event.Batch      as Batch
import qualified Event.TextEditor as TextEditor
import qualified JS.TextEditor    as UI

import qualified Reactive.State.Global           as Global
import           Reactive.Commands.Command       (Command, performIO)
import qualified BatchConnector.Monadic.Commands as BatchCmd

toAction :: Event -> Maybe (Command Global.State ())
toAction (Batch      (Batch.CodeUpdate        code)) = Just $ performIO $ UI.setText code
toAction (TextEditor (TextEditor.CodeModified code)) = Just $ zoom Global.workspace $ BatchCmd.setCode code
toAction _ = Nothing
