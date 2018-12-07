module NodeEditor.View.App where

import           Common.Prelude
import           NodeEditor.React.Model.App        (App, breadcrumbs, nodeEditor)
import           NodeEditor.React.Model.NodeEditor (applySearcherHints)
import           NodeEditor.View.Breadcrumb        (breadcrumbView)
import           NodeEditor.View.Diff              (diff, runDiffT)
import           NodeEditor.View.NodeEditor        (nodeEditorView)


appView :: MonadIO m => App -> App -> m ()
appView = runDiffT $ do
    diff breadcrumbView $ to convert
    diff nodeEditorView $ nodeEditor . to applySearcherHints
