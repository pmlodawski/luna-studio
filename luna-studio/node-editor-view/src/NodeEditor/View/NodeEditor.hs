module NodeEditor.View.NodeEditor where

import           Common.Prelude
import           Common.Data.JSON                  (fromJSONVal, toJSONVal)
import           LunaStudio.Data.Position          (Position, fromTuple)
import           NodeEditor.React.Model.NodeEditor (visualizersLibPaths, NodeEditor, nodeVisualizations, connections,
                                                    expressionNodes, halfConnections, inputNode,
                                                    outputNode, searcher, debugLayer)
import           NodeEditor.View.Connection        (connectionsView)
import           NodeEditor.View.Diff              (DiffT, diff, diffApply)
import           NodeEditor.View.ExpressionNode    (expressionNodesView)
import           NodeEditor.View.HalfConnection    (halfConnectionsView)
import           NodeEditor.View.Searcher          (searcherView)
import           NodeEditor.View.SidebarNode       (inputNodeView, outputNodeView)
import           NodeEditor.View.Visualization     (nodeVisualizationsView)
import           NodeEditor.View.VisualizerLibraries     (visualizerLibrariesView)


foreign import javascript safe
    "callback.getNodeEditorView().setDebugLayer($1)"
    setDebugLayer__ :: JSVal -> IO ()

foreign import javascript safe
    "callback.getNodeEditorView().unsetDebugLayer($1)"
    unsetDebugLayer__ :: JSVal -> IO ()

debugLayerView :: MonadIO m => DiffT (Maybe Int) m ()
debugLayerView = diffApply setDebugLayer

setDebugLayer :: MonadIO m => Maybe Int -> m ()
setDebugLayer l = liftIO $ setDebugLayer__ =<< toJSONVal l

unsetDebugLayer :: MonadIO m => Maybe Int -> m ()
unsetDebugLayer l = liftIO $ unsetDebugLayer__ =<< toJSONVal l


nodeEditorView :: MonadIO m => DiffT NodeEditor m ()
nodeEditorView = do
    diff expressionNodesView     expressionNodes
    diff inputNodeView           inputNode
    diff outputNodeView          outputNode
    diff searcherView            searcher
    diff connectionsView         connections
    diff halfConnectionsView     halfConnections
    diff nodeVisualizationsView  nodeVisualizations
    diff visualizerLibrariesView visualizersLibPaths
    diff debugLayerView          debugLayer

getMousePosition :: MonadIO m => m Position
getMousePosition = fmap (maybe def fromTuple) . fromJSONVal =<< liftIO getMousePosition'

foreign import javascript safe "callback.getNodeEditorView().getMousePosition()"
    getMousePosition' :: IO JSVal
