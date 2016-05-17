module Reactive.Plugins.Core.Action.TextEditor where

import           Utils.PreludePlus
import           Utils.Vector                    (Vector2 (..), x, y)

import qualified Event.Batch                     as Batch
import           Event.Event                     (Event (..))
import qualified Event.TextEditor                as TextEditor
import qualified Event.Window                    as Window
import qualified JS.TextEditor                   as UI
import           Reactive.Commands.Command       (Command, performIO)
import qualified Reactive.State.Camera           as Camera
import qualified Reactive.State.Global           as Global
import qualified Reactive.State.UIElements       as UIElements

import qualified Batch.Workspace                 as Workspace
import qualified BatchConnector.Monadic.Commands as BatchCmd
import           Empire.API.Data.GraphLocation   (GraphLocation)
import qualified Empire.API.Graph.CodeUpdate     as CodeUpdate


isCurrentLocation :: GraphLocation -> Command Global.State Bool
isCurrentLocation location = uses (Global.workspace . Workspace.currentLocation) (== location)


toAction :: Event -> Maybe (Command Global.State ())
toAction (Batch      (Batch.CodeUpdated response)) = Just $ do
    shouldProcess <- isCurrentLocation (response ^. CodeUpdate.location)
    when shouldProcess $
        performIO $ UI.setText $ response ^. CodeUpdate.code

-- toAction (TextEditor (TextEditor.CodeModified code))  = Just $ zoom Global.workspace $ BatchCmd.setCode code
toAction _                                            = Nothing

relayout :: Vector2 Int -> Command Global.State Int
relayout screenSize = do
    visible    <- use $ Global.uiElements . UIElements.textEditorVisible
    performIO $ UI.setVisible visible
    let width = (floor $ 0.3 * (fromIntegral $ screenSize ^. x))
    performIO $ UI.setWidth width

    return $ if visible then width else 0
