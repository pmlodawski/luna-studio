module BatchConnector.Commands where

import           Data.ByteString.Lazy.Char8 (pack)
import           Utils.PreludePlus
import qualified Data.Sequence              as Seq
import           Text.ProtocolBuffers       (Utf8(..), messagePut)
import           Text.ProtocolBuffers.Basic (uFromString)
import           BatchConnector.Connection  (sendMessage, sendMany, WebMessage(..))
import           Batch.Project              as Project
import           Batch.Library              as Library
import           Batch.Breadcrumbs
import           Data.Map                   as Map
import           BatchConnector.Conversion
import           Data.Int

import           Batch.Function
import           Batch.Workspace
import           Batch.Breadcrumbs
import           Object.Node
import           Object.Object

import qualified Generated.Proto.ProjectManager.Project.List.Request                 as ListProjects
import qualified Generated.Proto.ProjectManager.Project.Create.Request               as CreateProject
import qualified Generated.Proto.ProjectManager.Project.Store.Request                as StoreProject
import qualified Generated.Proto.ProjectManager.Project.Open.Request                 as OpenProject
import qualified Generated.Proto.ProjectManager.Project.Library.List.Request         as ListLibraries
import qualified Generated.Proto.ProjectManager.Project.Library.Create.Request       as CreateLibrary
import qualified Generated.Proto.ProjectManager.Project.Library.AST.Get.Request      as GetAST
import qualified Generated.Proto.ProjectManager.Project.Library.AST.Code.Get.Request as GetCode
import qualified Generated.Proto.ProjectManager.Project.Library.AST.Code.Set.Request as SetCode

import qualified Generated.Proto.ProjectManager.Project.Library.AST.Function.Add.Request                      as AddFunction
import qualified Generated.Proto.ProjectManager.Project.Library.AST.Function.Graph.Get.Request                as GetGraph
import qualified Generated.Proto.ProjectManager.Project.Library.AST.Function.Graph.Connect.Request            as Connect
import qualified Generated.Proto.ProjectManager.Project.Library.AST.Function.Graph.Disconnect.Request         as Disconnect
import qualified Generated.Proto.ProjectManager.Project.Library.AST.Function.Graph.Node.Add.Request           as AddNode
import qualified Generated.Proto.ProjectManager.Project.Library.AST.Function.Graph.Node.Remove.Request        as RemoveNode
import qualified Generated.Proto.ProjectManager.Project.Library.AST.Function.Graph.Node.ModifyInPlace.Request as ModifyNode
import           Generated.Proto.Dep.Version.Version
import           Generated.Proto.Dep.Attributes.Attributes
import           Generated.Proto.Mode.Mode
import qualified Generated.Proto.Dep.Crumb.Breadcrumbs                        as ProtoBreadcrumbs
import qualified Generated.Proto.Interpreter.Interpreter.SetProjectID.Request as SetProjectId
import qualified Generated.Proto.Interpreter.Interpreter.GetProjectID.Request as GetProjectId
import qualified Generated.Proto.Interpreter.Interpreter.Run.Request          as Run
import qualified Generated.Proto.Interpreter.Interpreter.SetMainPtr.Request   as SetMainPtr
import qualified Generated.Proto.Interpreter.DefPoint                         as DefPoint
import           Generated.Proto.Interpreter.CallPoint                        (CallPoint(..))
import           Generated.Proto.Interpreter.CallPointPath                    (CallPointPath(..))
import qualified Generated.Proto.Interpreter.Interpreter.Value.Request        as Value

import qualified Generated.Proto.Interpreter.Interpreter.SerializationMode.Insert.Request as InsertSerializationMode

import qualified Generated.Proto.Dep.Graph.Node.Cls     as NodeCls
import qualified Generated.Proto.Dep.Graph.Node         as GenNode
import qualified Generated.Proto.Dep.Graph.NodeExpr     as GenExpr
import qualified Generated.Proto.Dep.Graph.NodeExpr.Cls as ExprCls

uselessLegacyArgument :: Int32
uselessLegacyArgument = 42

createProject :: String -> String -> IO ()
createProject name path = sendMessage msg where
    msg  = WebMessage "project.create.request" $ messagePut body
    body = CreateProject.Request (Just $ uFromString name)
                                 (uFromString path)
                                 (Attributes Seq.empty)

listProjects :: IO ()
listProjects  = sendMessage msg where
    msg = WebMessage "project.list.request" $ messagePut ListProjects.Request

createLibrary :: String -> String -> Project -> IO ()
createLibrary name path project = sendMessage msg where
    msg  = WebMessage "project.library.create.request" $ messagePut body
    body = CreateLibrary.Request (uFromString name)
                                 (Version Seq.empty Seq.empty)
                                 (uFromString path)
                                 (project ^. Project.id)

fetchLibraries :: Project -> IO ()
fetchLibraries project = sendMessage msg where
    msg  = WebMessage "project.library.list.request" $ messagePut body
    body = ListLibraries.Request (project ^. Project.id)

setProjectId :: Project -> IO ()
setProjectId project = sendMessage msg where
    msg  = WebMessage "interpreter.setprojectid.request" $ messagePut body
    body = SetProjectId.Request (project ^. Project.id)

getProjectId :: IO ()
getProjectId  = sendMessage msg where
    msg  = WebMessage "interpreter.getprojectid.request" $ messagePut GetProjectId.Request

createFunction :: Project -> Library -> Breadcrumbs -> String -> IO ()
createFunction project library parent name = sendMessage msg where
    msg  = WebMessage "project.library.ast.function.add.request" $ messagePut body
    body = AddFunction.Request (emptyFunctionExpr name)
                               (encode parent)
                               (library ^. Library.id)
                               (project ^. Project.id)
                               uselessLegacyArgument

openProject :: String -> IO ()
openProject path = sendMessage msg where
    msg  = WebMessage "project.open.request" $ messagePut body
    body = OpenProject.Request (uFromString path)

storeProject :: Project -> IO ()
storeProject project = sendMessage msg where
    msg  = WebMessage "project.store.request" $ messagePut body
    body = StoreProject.Request (project ^. Project.id)
                                (Seq.fromList $ (view Library.id) <$> project ^. libs)
                                (Just $ uFromString $ project ^. Project.path)

runMain :: IO ()
runMain  = sendMessage msg where
    msg  = WebMessage "interpreter.run.request" $ messagePut $ Run.Request (Just 0.0)

setMainPtr :: Workspace -> IO ()
setMainPtr workspace = sendMessage msg where
    msg      = WebMessage "interpreter.setmainptr.request" $ messagePut body
    body     = SetMainPtr.Request defPoint
    defPoint = DefPoint.DefPoint (workspace ^. project . Project.id)
                                 (workspace ^. library . Library.id)
                                 (encode $ workspace ^. breadcrumbs)

getGraph :: Workspace -> IO ()
getGraph workspace = sendMessage msg where
    msg  = WebMessage "project.library.ast.function.graph.get.request" $ messagePut body
    body = GetGraph.Request (encode $ workspace ^. breadcrumbs)
                            (workspace ^. library . Library.id)
                            (workspace ^. project . Project.id)
                            uselessLegacyArgument

getCode :: Workspace -> IO ()
getCode workspace = sendMessage msg where
    msg  = WebMessage "project.library.ast.code.get.request" $ messagePut body
    body = GetCode.Request (encode $ workspace ^. breadcrumbs)
                           (workspace ^. library . Library.id)
                           (workspace ^. project . Project.id)
                           uselessLegacyArgument

addNode :: Workspace -> Node -> IO ()
addNode workspace node = sendMessage msg where
    msg  = WebMessage "project.library.ast.function.graph.node.add.request" $ messagePut body
    body = AddNode.Request (encode node)
                           (encode $ workspace ^. breadcrumbs)
                           (workspace ^. library . Library.id)
                           (workspace ^. project . Project.id)
                           uselessLegacyArgument

updateNodeMessage :: Workspace -> Node -> WebMessage
updateNodeMessage workspace node = WebMessage topic $ messagePut body where
    topic = "project.library.ast.function.graph.node.modifyinplace.request"
    body  = ModifyNode.Request (encode node)
                               (encode $ workspace ^. breadcrumbs)
                               (workspace ^. library . Library.id)
                               (workspace ^. project . Project.id)
                               uselessLegacyArgument

updateNodes :: Workspace -> [Node] -> IO ()
updateNodes workspace nodes = sendMany $ (updateNodeMessage workspace) <$> nodes

portRefToList :: PortId -> [Int]
portRefToList AllPorts     = []
portRefToList (PortNum id) = [id]

connectNodes :: Workspace -> PortRef -> PortRef -> IO ()
connectNodes workspace src dst = sendMessage msg where
    msg  = WebMessage "project.library.ast.function.graph.connect.request" $ messagePut body
    body = Connect.Request (encode $ src ^. refPortNodeId)
                           (encode . portRefToList $ src ^. refPortId)
                           (encode $ dst ^. refPortNodeId)
                           (encode . portRefToList $ dst ^. refPortId)
                           (encode $ workspace ^. breadcrumbs)
                           (workspace ^. library . Library.id)
                           (workspace ^. project . Project.id)
                           uselessLegacyArgument

disconnectMessage :: Workspace -> (PortRef, PortRef) -> WebMessage
disconnectMessage workspace (srcRef, dstRef) = WebMessage topic $ messagePut body where
    topic = "project.library.ast.function.graph.disconnect.request"
    body  = Disconnect.Request (encode $ srcRef ^. refPortNodeId)
                               (encode . portRefToList $ srcRef ^. refPortId)
                               (encode $ dstRef ^. refPortNodeId)
                               (encode . portRefToList $ dstRef ^. refPortId)
                               (encode $ workspace ^. breadcrumbs)
                               (workspace ^. library . Library.id)
                               (workspace ^. project . Project.id)
                               uselessLegacyArgument

disconnectNodes :: Workspace -> [(PortRef, PortRef)] -> IO ()
disconnectNodes workspace connections = sendMany $ (disconnectMessage workspace) <$> connections

nodeToCallPointPath :: Workspace -> Node -> CallPointPath
nodeToCallPointPath workspace node = CallPointPath projectId (Seq.fromList [callPoint]) where
    projectId     = workspace ^. project . Project.id
    callPoint     = CallPoint (workspace ^. library . Library.id) (encode $ node ^. nodeId)

requestValueMessage :: Workspace -> Node -> WebMessage
requestValueMessage workspace node = WebMessage "interpreter.value.request" $ messagePut body where
    body = Value.Request (nodeToCallPointPath workspace node) 0.0

requestValue :: Workspace -> Node -> IO ()
requestValue = sendMessage .: requestValueMessage

requestValues :: Workspace -> [Node] -> IO ()
requestValues workspace nodes = sendMany $ (requestValueMessage workspace) <$> nodes

insertSerializationModeMessage :: Workspace -> Node -> WebMessage
insertSerializationModeMessage workspace node = WebMessage topic $ messagePut body where
    topic         = "interpreter.serializationmode.insert.request"
    body          = InsertSerializationMode.Request callPointPath (Seq.fromList [mode])
    callPointPath = nodeToCallPointPath workspace node
    mode          = Mode Seq.empty

insertSerializationMode :: Workspace -> Node -> IO ()
insertSerializationMode workspace node = sendMessage $ insertSerializationModeMessage workspace node

insertSerializationModes :: Workspace -> [Node] -> IO ()
insertSerializationModes workspace nodes = sendMany $ insertSerializationModeMessage workspace <$> nodes

getAST :: Project -> Library -> Breadcrumbs -> IO ()
getAST proj lib crumbs = sendMessage msg where
    msg  = WebMessage "project.library.ast.get.request" $ messagePut body
    body = GetAST.Request Nothing
                          (encode crumbs)
                          (lib ^. Library.id)
                          (proj ^. Project.id)
                          uselessLegacyArgument

removeNodeById :: Workspace -> Int -> IO ()
removeNodeById workspace nodeId = sendMessage msg where
    msg  = WebMessage "project.library.ast.function.graph.node.remove.request" $ messagePut body
    body = RemoveNode.Request (encode [nodeId])
                              (encode $ workspace ^. breadcrumbs)
                              (workspace ^. library . Library.id)
                              (workspace ^. project . Project.id)
                              uselessLegacyArgument

setCode :: Workspace -> Text -> IO ()
setCode workspace code = sendMessage msg where
    msg  = WebMessage "project.library.ast.code.set.request" $ messagePut body
    body = SetCode.Request (encode code)
                           (encode $ workspace ^. breadcrumbs)
                           (workspace ^. library . Library.id)
                           (workspace ^. project . Project.id)
                           uselessLegacyArgument
