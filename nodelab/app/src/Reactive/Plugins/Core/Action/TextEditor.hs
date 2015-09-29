module Reactive.Plugins.Core.Action.TextEditor where


import           Utils.PreludePlus
import           Utils.Vector
import           Utils.Angle


import           Event.Event
import qualified Event.Batch      as Batch
import qualified Event.TextEditor as TextEditor
import           Object.Object
import           Object.Node
import           Object.UITypes

import qualified BatchConnector.Commands as BatchCmd
import qualified JS.TextEditor   as UI

import           Reactive.Plugins.Core.Action
import           Reactive.Plugins.Core.Action.State.Graph
import qualified Reactive.Plugins.Core.Action.State.Global        as Global

data Action = CodeUpdate   Text
            | CodeModified Text
            deriving (Show, Eq)

instance PrettyPrinter Action where
    display v = "gCE(" <> show v <> ")"

toAction :: Event Node -> Maybe Action
toAction (Batch      (Batch.CodeUpdate        code)) = Just $ CodeUpdate code
toAction (TextEditor (TextEditor.CodeModified code)) = Just $ CodeModified code
toAction _ = Nothing

instance ActionStateUpdater Action where
    execSt action state = ActionUI action state
instance ActionUIUpdater Action where
    updateUI (WithState (CodeUpdate   code) _)     = UI.setText code
    updateUI (WithState (CodeModified code) state) = do putStrLn $ "Updating code to " <> (show code)
                                                        BatchCmd.setCode (state ^. Global.workspace) code

