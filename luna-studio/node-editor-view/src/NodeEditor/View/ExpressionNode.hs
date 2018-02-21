module NodeEditor.View.ExpressionNode where

import           Common.Data.JSON                           (toJSONVal)
import           Common.Prelude
import           Data.Aeson                                 (ToJSON)
import           Data.Convert                               (Convertible (convert))
import qualified Data.HashMap.Strict                        as HashMap
import           LunaStudio.Data.Position                   (toTuple)
import           NodeEditor.React.Model.Node.ExpressionNode (ExpressionNode, ExpressionNodesMap)
import qualified NodeEditor.React.Model.Node.ExpressionNode as ExpressionNode
import           NodeEditor.View.Port                       (PortView(PortView))

expressionNodesView :: MonadIO m => ExpressionNodesMap -> ExpressionNodesMap -> m ()
expressionNodesView new old =
    when (new /= old) $
        setNodes $ map convert $ HashMap.elems new

data ExpressionNodeView = ExpressionNodeView
        { key        :: String
        , name       :: String
        , expression :: String
        , inPorts    :: [PortView]
        , outPorts   :: [PortView]
        , position   :: (Double, Double)
        , expanded   :: Bool
        , selected   :: Bool
        } deriving (Generic, Show)

instance ToJSON ExpressionNodeView
instance Convertible ExpressionNode ExpressionNodeView where
    convert n = ExpressionNodeView
        {- key        -} (n ^. ExpressionNode.nodeId . to show)
        {- name       -} (n ^. ExpressionNode.name . to convert . to (fromMaybe def))
        {- expression -} (n ^. ExpressionNode.expression . to convert)
        {- inPorts    -} (n ^. to ExpressionNode.inPortsList . to convert)
        {- outPorts   -} (n ^. to ExpressionNode.outPortsList . to convert)
        {- position   -} (n ^. ExpressionNode.position . to toTuple)
        {- expanded   -} (n ^. ExpressionNode.mode == ExpressionNode.Expanded ExpressionNode.Controls)
        {- selected   -} (n ^. ExpressionNode.isSelected)

foreign import javascript safe "atomCallback.getNodeEditorView().setNodes($1)"
    setNodes' :: JSVal -> IO ()

setNodes :: MonadIO m => [ExpressionNodeView] -> m ()
setNodes = liftIO . setNodes' <=< toJSONVal
