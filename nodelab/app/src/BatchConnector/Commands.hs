module BatchConnector.Commands where

import           Data.ByteString.Lazy.Char8 (pack)
import           Utils.PreludePlus
import qualified Data.Sequence              as Seq
import           Text.ProtocolBuffers       (Utf8(..), messagePut)
import           Text.ProtocolBuffers.Basic (uFromString)
import           BatchConnector.Connection
import           Batch.Project              as Project
import           Batch.Library              as Library
import           Batch.Breadcrumbs
import           Data.Map                   as Map
import           BatchConnector.Conversion

import           Batch.Function
import           Batch.Workspace
import           Object.Node

import qualified Generated.Proto.ProjectManager.Project.List.Request                                as ListProjects
import qualified Generated.Proto.ProjectManager.Project.Create.Request                              as CreateProject
import qualified Generated.Proto.ProjectManager.Project.Library.Create.Request                      as CreateLibrary
import qualified Generated.Proto.ProjectManager.Project.Library.List.Request                        as ListLibraries
import qualified Generated.Proto.ProjectManager.Project.Library.AST.Function.Add.Request            as AddFunction
import qualified Generated.Proto.ProjectManager.Project.Library.AST.Code.Get.Request                as GetCode
import qualified Generated.Proto.ProjectManager.Project.Library.AST.Function.Graph.Get.Request      as GetGraph
import qualified Generated.Proto.ProjectManager.Project.Library.AST.Function.Graph.Node.Add.Request as AddNode
import qualified Generated.Proto.ProjectManager.Project.Library.AST.Function.Graph.Connect.Request  as Connect
import           Generated.Proto.Dep.Version.Version
import           Generated.Proto.Dep.Attributes.Attributes
import           Generated.Proto.Dep.Module.Module
import           Generated.Proto.Mode.Mode
import qualified Generated.Proto.Dep.Crumb.Breadcrumbs                        as ProtoBreadcrumbs
import qualified Generated.Proto.Interpreter.Interpreter.SetProjectID.Request as SetProjectId
import qualified Generated.Proto.Interpreter.Interpreter.Run.Request          as Run
import qualified Generated.Proto.Interpreter.Interpreter.SetMainPtr.Request   as SetMainPtr
import qualified Generated.Proto.Interpreter.DefPoint                         as DefPoint
import           Generated.Proto.Interpreter.CallPoint                        (CallPoint(..))
import           Generated.Proto.Interpreter.CallPointPath                    (CallPointPath(..))

import qualified Generated.Proto.Interpreter.Interpreter.SerializationMode.Insert.Request as InsertSerializationMode

import qualified Generated.Proto.Dep.Graph.Node.Cls     as NodeCls
import qualified Generated.Proto.Dep.Graph.Node         as GenNode
import qualified Generated.Proto.Dep.Graph.NodeExpr     as GenExpr
import qualified Generated.Proto.Dep.Graph.NodeExpr.Cls as ExprCls


createProject :: String -> String -> WebMessage
createProject name path = WebMessage "project.create.request" $ messagePut body where
    body = CreateProject.Request (Just $ uFromString name)
                                 (uFromString path)
                                 (Attributes Seq.empty)

listProjects :: WebMessage
listProjects  = WebMessage "project.list.request" $ messagePut ListProjects.Request

createLibrary :: String -> String -> Project -> WebMessage
createLibrary name path project = WebMessage "project.library.create.request" $ messagePut body where
    body = CreateLibrary.Request (uFromString name)
                                 (Version Seq.empty Seq.empty)
                                 (uFromString path)
                                 (project ^. Project.id)

fetchLibraries :: Project -> WebMessage
fetchLibraries project = WebMessage "project.library.list.request" $ messagePut body where
    body = ListLibraries.Request (project ^. Project.id)

setProjectId :: Project -> WebMessage
setProjectId project = WebMessage "interpreter.setprojectid.request" $ messagePut body where
    body = SetProjectId.Request (project ^. Project.id)

createMainFunction :: Project -> Library -> WebMessage
createMainFunction project library = WebMessage "project.library.ast.function.add.request" $ messagePut body where
    body = AddFunction.Request emptyFunctionExpr
                               (encode $ moduleBreadcrumbs "Main")
                               (library ^. Library.id)
                               (project ^. Project.id)
                               1

runMain :: WebMessage
runMain  = WebMessage "interpreter.run.request" $ messagePut $ Run.Request Nothing

setMainPtr :: Project -> Library -> Breadcrumbs -> WebMessage
setMainPtr proj lib crumbs = WebMessage "interpreter.setmainptr.request" $ messagePut body where
    body     = SetMainPtr.Request defPoint
    defPoint = DefPoint.DefPoint (proj ^. Project.id)
                                 (lib  ^. Library.id)
                                 (encode crumbs)

getGraph :: Project -> Library -> Breadcrumbs -> WebMessage
getGraph proj lib crumbs = WebMessage "project.library.ast.function.graph.get.request" $ messagePut body where
    body = GetGraph.Request (encode crumbs)
                            (lib  ^. Library.id)
                            (proj ^. Project.id)
                            1

getCode :: Workspace -> WebMessage
getCode workspace = WebMessage "project.library.ast.code.get.request" $ messagePut body where
    body = GetCode.Request (encode $ workspace ^. breadcrumbs)
                           (workspace ^. library . Library.id)
                           (workspace ^. project . Project.id)
                           1

addNode :: Workspace -> Node -> WebMessage
addNode workspace node = WebMessage "project.library.ast.function.graph.node.add.request" $ messagePut body where
    body = AddNode.Request (encode node)
                           (encode $ workspace ^. breadcrumbs)
                           (workspace ^. project . Project.id)
                           (workspace ^. library . Library.id)
                           1

connectNodes :: Workspace -> PortRef -> PortRef -> WebMessage
connectNodes workspace src dst = connectNodes' workspace
                                               (src ^. refPortNode . nodeId)
                                               []
                                               (dst ^. refPortNode . nodeId)
                                               [dst ^. refPortId]

-- TODO: Remove - low level debug interface
connectNodes' :: Workspace -> Int -> [Int] -> Int -> [Int] -> WebMessage
connectNodes' workspace srcNode srcPorts dstNode dstPorts = WebMessage "project.library.ast.function.graph.connect.request" $ messagePut body where
    body = Connect.Request (encode srcNode)
                           (encode srcPorts)
                           (encode dstNode)
                           (encode dstPorts)
                           (encode $ workspace ^. breadcrumbs)
                           (workspace ^. library . Library.id)
                           (workspace ^. project . Project.id)
                           1

insertSerializationMode :: Workspace -> Node -> WebMessage
insertSerializationMode workspace node = WebMessage "interpreter.serializationmode.insert.request" $ messagePut body where
    body          = InsertSerializationMode.Request callPointPath (Seq.fromList [mode])
    callPointPath = CallPointPath (workspace ^. project . Project.id) (Seq.fromList [callPoint])
    callPoint     = CallPoint (workspace ^. library . Library.id) (encode $ node ^. nodeId)
    mode          = Mode Seq.empty
