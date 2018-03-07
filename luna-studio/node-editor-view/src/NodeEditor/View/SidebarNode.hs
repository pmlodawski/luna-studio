module NodeEditor.View.SidebarNode where

import           Common.Data.JSON                        (toJSONVal)
import           Common.Prelude
import           Data.Aeson                              (ToJSON)
import           Data.Convert                            (Convertible (convert))
import           LunaStudio.Data.Position                (toTuple)
import           NodeEditor.React.Model.Node.SidebarNode (InputNode, OutputNode)
import qualified NodeEditor.React.Model.Node.SidebarNode as SidebarNode
import           NodeEditor.View.Port                    (PortView)

inputNodeView :: MonadIO m => Maybe InputNode -> Maybe InputNode -> m ()
inputNodeView new old =
    when (new /= old) $
        setInputNode $ convert new

outputNodeView :: MonadIO m => Maybe OutputNode -> Maybe OutputNode -> m ()
outputNodeView new old =
    when (new /= old) $
        setOutputNode $ convert new


data SidebarNodeView = SidebarNodeView
        { key      :: String
        , inPorts  :: [PortView]
        , outPorts :: [PortView]
        } deriving (Generic, Show)

instance ToJSON SidebarNodeView
instance Convertible InputNode SidebarNodeView where
    convert n = SidebarNodeView
        {- key      -} (n ^. SidebarNode.nodeLoc . to show)
        {- inPorts  -} def
        {- outPorts -} (n ^. to SidebarNode.outPortsList . to convert)

instance Convertible OutputNode SidebarNodeView where
    convert n = SidebarNodeView
        {- key      -} (n ^. SidebarNode.nodeLoc . to show)
        {- inPorts  -} (n ^. to SidebarNode.inPortsList . to convert)
        {- outPorts -} def

foreign import javascript safe "atomCallback.getNodeEditorView().setInputNode($1)"
    setInputNode' :: JSVal -> IO ()

foreign import javascript safe "atomCallback.getNodeEditorView().setOutputNode($1)"
    setOutputNode' :: JSVal -> IO ()

setInputNode :: MonadIO m => Maybe SidebarNodeView -> m ()
setInputNode = liftIO . setInputNode' <=< toJSONVal

setOutputNode :: MonadIO m => Maybe SidebarNodeView -> m ()
setOutputNode = liftIO . setOutputNode' <=< toJSONVal
