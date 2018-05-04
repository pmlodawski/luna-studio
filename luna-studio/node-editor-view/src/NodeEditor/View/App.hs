module NodeEditor.View.App where

import           Common.Prelude
import           NodeEditor.React.Model.App  (App, breadcrumbs, nodeEditor)
import           NodeEditor.View.Breadcrumbs (breadcrumbsView)
import           NodeEditor.View.Diff        (diff, runDiffT)
import           NodeEditor.View.NodeEditor  (nodeEditorView)
import           NodeEditor.React.View.NodeEditor (applySearcherHints)


appView :: MonadIO m => App -> App -> m ()
appView = runDiffT $ transaction $ do
    diff breadcrumbsView $ to convert
    diff nodeEditorView  $ nodeEditor . to applySearcherHints

foreign import javascript safe "atomCallback.getNodeEditorView().beginTransaction()"
    beginTransaction__ :: IO ()

foreign import javascript safe "atomCallback.getNodeEditorView().commitTransaction()"
    commitTransaction__ :: IO ()

transaction :: MonadIO m => m () -> m ()
transaction action = do
    liftIO beginTransaction__
    action
    liftIO commitTransaction__
