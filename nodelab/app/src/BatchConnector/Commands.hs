module BatchConnector.Commands where

import           Utils.PreludePlus

import           Data.ByteString.Lazy.Char8 (pack)
import qualified Data.Sequence              as Seq
import           Data.Map                   as Map
import           Data.Int
import qualified Data.Text.Lazy as Text
import qualified Data.Text.Lazy as Text

import           Utils.Vector (Vector2(..), x, y)

-- import           Text.ProtocolBuffers       (Utf8(..), messagePut)
-- import           Text.ProtocolBuffers.Basic (uFromString)

import           Batch.Workspace (Workspace)
import qualified Batch.Workspace as Workspace
import           Batch.Expressions
import           Batch.Breadcrumbs
import           Empire.API.Data.Node (Node(..))
import qualified Empire.API.Data.Node as Node
import qualified Empire.API.Data.Port as Port
import           Empire.API.Data.PortRef (InPortRef(..), OutPortRef(..))
import qualified Empire.API.Data.PortRef as PortRef
import qualified Empire.API.Data.Connection as Connection
import qualified Empire.API.Topic as Topic
import           BatchConnector.Connection  (sendMessage, sendMany, sendRequest, WebMessage(..))

import           Empire.API.Data.NodeMeta (NodeMeta)
import qualified Empire.API.Data.NodeMeta as NodeMeta
import           Empire.API.Data.Node (NodeId)
import           Empire.API.Data.Port (InPort(..))
import           Empire.API.Data.Project (ProjectId, Project)
import qualified Empire.API.Data.Project as Project
import           Empire.API.Data.GraphLocation (GraphLocation)
import qualified Empire.API.Data.GraphLocation as GraphLocation
import           Empire.API.Data.Library (LibraryId, Library)
import qualified Empire.API.Data.Library as Library
import qualified Empire.API.Data.DefaultValue as DefaultValue
import qualified Empire.API.Topic        as Topic

import Empire.API.Graph.AddNode         as AddNode
import Empire.API.Graph.Connect         as Connect
import Empire.API.Graph.Disconnect      as Disconnect
import Empire.API.Graph.RemoveNode      as RemoveNode
import Empire.API.Graph.UpdateNodeMeta  as UpdateNodeMeta
import Empire.API.Graph.SetDefaultValue as SetDefaultValue

import Empire.API.Graph.GetProgram     as GetProgram

import Empire.API.Project.CreateProject as CreateProject
import Empire.API.Project.ListProjects  as ListProjects

import Empire.API.Library.CreateLibrary  as CreateLibrary
import Empire.API.Library.ListLibraries  as ListLibraries


import Data.Binary (encode)
import qualified Data.Binary as Binary

withLibrary :: Workspace -> (GraphLocation -> a) -> a
withLibrary w f = f (w ^. Workspace.currentLocation)

addNode :: Workspace -> Text -> NodeMeta -> Int -> IO ()
addNode workspace expression meta tag = sendRequest topic body where
    topic = Topic.addNodeRequest
    body  = (withLibrary workspace AddNode.Request) (Text.unpack expression)
                                                   meta
                                                   tag

createProject :: Text -> Text -> IO ()
createProject name path = sendRequest topic body where
    topic = Topic.createProjectRequest
    body  = CreateProject.Request (Just $ Text.unpack name)
                                  (Text.unpack path)

listProjects :: IO ()
listProjects = sendRequest Topic.listProjectsRequest ListProjects.Request

createLibrary :: Workspace -> Text -> Text -> IO ()
createLibrary w name path = sendRequest topic body where
    topic = Topic.createLibraryRequest
    body  = CreateLibrary.Request (w ^. Workspace.currentLocation . GraphLocation.projectId)
                                  (Just $ Text.unpack name)
                                  (Text.unpack path)

listLibraries :: ProjectId -> IO ()
listLibraries projectId = sendRequest Topic.listLibrariesRequest $ ListLibraries.Request projectId

getProgram :: Workspace -> IO ()
getProgram workspace = sendRequest Topic.programRequest $ withLibrary workspace GetProgram.Request

updateNodeMeta :: Workspace -> NodeId -> NodeMeta -> IO ()
updateNodeMeta w nid nm = sendRequest Topic.updateNodeMetaRequest $ withLibrary w UpdateNodeMeta.Request nid nm

removeNode :: Workspace -> NodeId -> IO ()
removeNode workspace nid = sendRequest topic body where
    topic = Topic.removeNodeRequest
    body  = withLibrary workspace RemoveNode.Request nid


connectNodes :: Workspace -> OutPortRef -> InPortRef -> IO ()
connectNodes workspace src dst = sendRequest topic body where
    topic = Topic.connectRequest
    body = (withLibrary workspace Connect.Request) (src ^. PortRef.srcNodeId)
                                                   (src ^. PortRef.srcPortId)
                                                   (dst ^. PortRef.dstNodeId)
                                                   (dst ^. PortRef.dstPortId)

disconnectMessage :: Workspace -> (OutPortRef, InPortRef) -> WebMessage
disconnectMessage workspace (src, dst) = WebMessage Topic.disconnectRequest $ encode body where
    body  = (withLibrary workspace Disconnect.Request) (dst ^. PortRef.dstNodeId)
                                                       (dst ^. PortRef.dstPortId)

disconnectNodes :: Workspace -> [(OutPortRef, InPortRef)] -> IO ()
disconnectNodes workspace connections = sendMany $ (disconnectMessage workspace) <$> connections

setDefaultValue :: Workspace -> NodeId -> InPort -> Maybe DefaultValue.PortDefault -> IO ()
setDefaultValue workspace nodeId portId val = sendRequest topic body where
    topic = Topic.setDefaultValueRequest
    body = (withLibrary workspace SetDefaultValue.Request) nodeId portId val
-----

setProjectId :: Project -> IO ()
setProjectId project = return () -- sendMessage msg where
    -- msg  = WebMessage "interpreter.setprojectid.request" $ messagePut body
    -- body = SetProjectId.Request (project ^. Project.id)

getProjectId :: IO ()
getProjectId  = return () --sendMessage msg where
    -- msg  = WebMessage "interpreter.getprojectid.request" $ messagePut GetProjectId.Request

-- createFunction :: Project -> Library -> Breadcrumbs -> String -> IO ()
-- createFunction project library parent name = return () -- sendMessage msg where
    -- msg  = WebMessage "project.library.ast.function.add.request" $ messagePut body
    -- body = AddFunction.Request (emptyFunctionExpr name)
    --                            (encode parent)
    --                            (library ^. Library.id)
    --                            (project ^. Project.id)
    --                            uselessLegacyArgument

openProject :: String -> IO ()
openProject path = return () -- sendMessage msg where
    -- msg  = WebMessage "project.open.request" $ messagePut body
    -- body = OpenProject.Request (uFromString path)

storeProject :: Project -> IO ()
storeProject project = return () -- sendMessage msg where
    -- msg  = WebMessage "project.store.request" $ messagePut body
    -- body = StoreProject.Request (project ^. Project.id)
    --                             (Seq.fromList $ (view Library.id) <$> project ^. libs)
    --                             (Just $ uFromString $ project ^. Project.path)

runMain :: IO ()
runMain  = return () -- sendMessage msg where
    -- msg  = WebMessage "interpreter.run.request" $ messagePut $ Run.Request (Just 0.0)

setMainPtr :: Workspace -> IO ()
setMainPtr workspace = return () -- sendMessage msg where
    -- msg      = WebMessage "interpreter.setmainptr.request" $ messagePut body
    -- body     = SetMainPtr.Request defPoint
    -- defPoint = DefPoint.DefPoint (workspace ^. project . Project.id)
    --                              (workspace ^. library . Library.id)
    --                              (encode $ workspace ^. breadcrumbs)
    --
--
--
-- updateNodeMessage :: Workspace -> Node -> WebMessage
-- updateNodeMessage workspace node = WebMessage topic $ messagePut body where
--     topic = "project.library.ast.function.graph.node.modifyinplace.request"
--     body  = ModifyNode.Request (encode node)
--                                (encode $ workspace ^. breadcrumbs)
--                                (workspace ^. library . Library.id)
--                                (workspace ^. project . Project.id)
--                                uselessLegacyArgument
--
updateNodes :: Workspace -> [Node] -> IO ()
updateNodes _ _ = return ()

updateNode :: Workspace -> Node -> IO ()
updateNode _ _ = return ()
-- updateNodes workspace nodes = sendMany $ (updateNodeMessage workspace) <$> nodes
--
-- updateNode :: Node -> Workspace -> IO ()
-- updateNode node workspace = sendMessage $ updateNodeMessage workspace node

-- portRefToList :: PortId -> [Int]
-- portRefToList AllPorts     = []
-- portRefToList (PortNum id) = [id]

-- portIdToListIn  Port.Self    = []
-- portIdToListIn (Port.Arg x) = [x]
-- portIdToListOut  Port.All    = []
-- portIdToListOut (Port.Projection x) = [x]

-- nodeToCallPointPath :: Workspace -> Node -> CallPointPath
-- nodeToCallPointPath workspace node = return () -- CallPointPath projectId (Seq.fromList [callPoint]) where
    -- projectId     = workspace ^. project . Project.id
    -- callPoint     = CallPoint (workspace ^. library . Library.id) (encode $ node ^. Node.nodeId)

-- requestValueMessage :: Workspace -> Node -> WebMessage
-- requestValueMessage workspace node = return () --WebMessage "interpreter.value.request" $ messagePut body where
    -- body = Value.Request (nodeToCallPointPath workspace node) 0.0

requestValue :: Workspace -> Node -> IO ()
requestValue _ _  = return () -- sendMessage .: requestValueMessage

requestValues :: [Node] -> Workspace -> IO ()
requestValues nodes workspace = return () -- sendMany $ (requestValueMessage workspace) <$> nodes

-- insertSerializationModeMessage :: Workspace -> Node -> WebMessage
-- insertSerializationModeMessage workspace node = return () -- WebMessage topic $ messagePut body where
    -- topic         = "interpreter.serializationmode.insert.request"
    -- body          = InsertSerializationMode.Request callPointPath (Seq.fromList [mode])
    -- callPointPath = nodeToCallPointPath workspace node
    -- mode          = Mode Seq.empty

insertSerializationMode :: Node -> Workspace -> IO ()
insertSerializationMode node workspace = return () -- sendMessage $ insertSerializationModeMessage workspace node

insertSerializationModes :: [Node] -> Workspace -> IO ()
insertSerializationModes nodes workspace = return () -- sendMany $ insertSerializationModeMessage workspace <$> nodes

-- getAST :: Project -> Library -> Breadcrumbs -> IO ()
-- getAST proj lib crumbs = return () -- sendMessage msg where
    -- msg  = WebMessage "project.library.ast.get.request" $ messagePut body
    -- body = GetAST.Request Nothing
    --                       (encode crumbs)
    --                       (lib ^. Library.id)
    --                       (proj ^. Project.id)
    --                       uselessLegacyArgument
--
-- setImports :: Project -> Library -> Breadcrumbs -> [([String], String)] -> IO ()
-- setImports proj lib crumbs imports = return () -- sendMessage msg where
    -- msg  = WebMessage "project.library.ast.module.modify.imports.request" $ messagePut body
    -- body = ModifyImports.Request (Seq.fromList $ uncurry importExpr <$> imports)
    --                              (encode crumbs)
    --                              (lib ^. Library.id)
    --                              (proj ^. Project.id)
    --                              uselessLegacyArgument
    --
-- removeNodeById :: Workspace -> Int -> IO ()
-- removeNodeById workspace nodeId = sendMessage msg where
--     msg  = WebMessage "project.library.ast.function.graph.node.remove.request" $ messagePut body
--     body = RemoveNode.Request (encode [nodeId])
--                               (encode $ workspace ^. breadcrumbs)
--                               (workspace ^. library . Library.id)
--                               (workspace ^. project . Project.id)
--                               uselessLegacyArgument

setCode :: Text -> Workspace -> IO ()
setCode code workspace = return () -- sendMessage msg where
    -- msg  = WebMessage "project.library.ast.code.set.request" $ messagePut body
    -- body = SetCode.Request (encode code)
    --                        (encode $ workspace ^. breadcrumbs)
    --                        (workspace ^. library . Library.id)
    --                        (workspace ^. project . Project.id)
    --                        uselessLegacyArgument

setValue :: Workspace -> InPortRef -> String -> IO ()
setValue workspace portRef value = return () -- sendMessage msg where
    -- msg  = WebMessage "project.library.ast.function.graph.node.default.set.request" $ messagePut body
    -- body = SetDefault.Request (encode . portIdToListIn $ portRef ^. PortRef.dstPortId)
    --                           (GenExpr.NodeExpr ExprCls.String (Just encodedVal) Nothing)
    --                           (encode $ portRef ^. PortRef.dstNodeId)
    --                           (encode $ workspace ^. breadcrumbs)
    --                           (workspace ^. library . Library.id)
    --                           (workspace ^. project . Project.id)
    --                           uselessLegacyArgument
    -- encodedVal = (uFromString value)
