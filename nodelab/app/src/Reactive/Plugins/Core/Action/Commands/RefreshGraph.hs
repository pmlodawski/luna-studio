module Reactive.Plugins.Core.Action.Commands.RefreshGraph where

import           Utils.PreludePlus
import qualified BatchConnector.Commands                             as BatchCmd
import qualified Reactive.Plugins.Core.Action.State.Global           as Global
import           Reactive.Plugins.Core.Action.State.Global           (State)
import           Reactive.Plugins.Core.Action.Commands.Command       (Command, performIO)
import           Reactive.Plugins.Core.Action.Commands.UnrenderGraph (unrender)

refreshGraph :: Command State ()
refreshGraph = do
    unrender
    workspace <- use Global.workspace
    performIO $ BatchCmd.getGraph workspace
