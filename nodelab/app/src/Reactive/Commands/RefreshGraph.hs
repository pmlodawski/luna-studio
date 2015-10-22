module Reactive.Commands.RefreshGraph where

import           Utils.PreludePlus
import qualified Batch.Workspace                                     as Workspace
import qualified BatchConnector.Commands                             as BatchCmd
import qualified Reactive.State.Global           as Global
import           Reactive.State.Global           (State)
import           Reactive.Commands.Command       (Command, performIO)
import           Reactive.Commands.UnrenderGraph (unrender)

refreshGraph :: Command State ()
refreshGraph = do
    unrender
    Global.workspace . Workspace.shouldLayout .= True
    Global.workspace . Workspace.interpreterState .= Workspace.Fresh
    workspace <- use Global.workspace
    performIO $ BatchCmd.getGraph workspace
