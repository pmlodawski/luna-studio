module NodeEditor.View.NodeEditor where

import           Common.Prelude
import           Common.Data.JSON                  (fromJSONVal)
import           LunaStudio.Data.Position          (Position, fromTuple)
import           NodeEditor.React.Model.NodeEditor (visualizersLibPaths, NodeEditor, activeNodeVisualizations, connections,
                                                    expressionNodes, halfConnections, inputNode,
                                                    outputNode, searcher)
import           NodeEditor.View.Connection        (connectionsView)
import           NodeEditor.View.Diff              (DiffT, diff)
import           NodeEditor.View.ExpressionNode    (expressionNodesView)
import           NodeEditor.View.HalfConnection    (halfConnectionsView)
import           NodeEditor.View.Searcher          (searcherView)
import           NodeEditor.View.SidebarNode       (inputNodeView, outputNodeView)
import           NodeEditor.View.Visualization     (nodeVisualizationsView)
import           NodeEditor.View.VisualizerLibraries     (visualizerLibrariesView)


nodeEditorView :: MonadIO m => DiffT NodeEditor m ()
nodeEditorView = do
    diff expressionNodesView     expressionNodes
    diff inputNodeView           inputNode
    diff outputNodeView          outputNode
    diff searcherView            searcher
    diff connectionsView         connections
    diff halfConnectionsView     halfConnections
    diff nodeVisualizationsView  activeNodeVisualizations
    diff visualizerLibrariesView visualizersLibPaths

getMousePosition :: MonadIO m => m Position
getMousePosition = fmap (maybe def fromTuple) . fromJSONVal =<< liftIO getMousePosition'

foreign import javascript safe "atomCallback.getNodeEditorView().getMousePosition()"
    getMousePosition' :: IO JSVal
