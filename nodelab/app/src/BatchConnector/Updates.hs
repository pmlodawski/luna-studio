module BatchConnector.Updates where

import           Utils.PreludePlus
import           Data.ByteString.Lazy
import qualified Data.Sequence              as Seq
import           Text.ProtocolBuffers
import           Text.ProtocolBuffers.Basic (uToString)

import           Batch.Project
import           Batch.Library
import           Batch.Breadcrumbs
import           Object.Node

import           BatchConnector.Conversion  (decode)

import qualified Generated.Proto.Dep.Graphview.GraphView                      as GraphView
import qualified Generated.Proto.ProjectManager.Project.List.Status           as ProjectsList
import qualified Generated.Proto.ProjectManager.Project.Create.Update         as ProjectCreated
import qualified Generated.Proto.ProjectManager.Project.Library.List.Status   as LibsList
import qualified Generated.Proto.ProjectManager.Project.Library.Create.Update as LibCreated
import qualified Generated.Proto.ProjectManager.Project.Library.AST.Function.Add.Update            as FunctionCreated
import qualified Generated.Proto.ProjectManager.Project.Library.AST.Function.Graph.Get.Status      as GraphViewResponse
import qualified Generated.Proto.ProjectManager.Project.Library.AST.Code.Get.Status                as GetCode
import qualified Generated.Proto.Interpreter.Interpreter.Value.Update                              as Value
import qualified Generated.Proto.ProjectManager.Project.Library.AST.Function.Graph.Node.Add.Update as AddNode
import qualified Generated.Proto.ProjectManager.Project.Library.AST.Function.Graph.Node.Add.Request as AddNodeReq

parseMessage :: (Wire m, ReflectDescriptor m) => ByteString -> Maybe m
parseMessage bytes = case messageGet bytes of
    Left  _        -> Nothing
    Right (msg, _) -> Just msg

pluckProjects :: ProjectsList.Status -> Maybe [Project]
pluckProjects  = decode . ProjectsList.projects

parseProjectsList :: ByteString -> Maybe [Project]
parseProjectsList bytes = (parseMessage bytes) >>= pluckProjects

parseProjectCreateUpdate :: ByteString -> Maybe Project
parseProjectCreateUpdate bytes = (parseMessage bytes) >>= getProject where
    getProject = decode . ProjectCreated.project

parseLibrariesListResponse :: ByteString -> Maybe [Library]
parseLibrariesListResponse bytes = (parseMessage bytes) >>= getLibs where
    getLibs = decode . LibsList.libraries

parseLibraryCreateResponse :: ByteString -> Maybe Library
parseLibraryCreateResponse bytes = (parseMessage bytes) >>= getLib where
    getLib = decode . LibCreated.library

parseFunctionCreateResponse :: ByteString -> Maybe Breadcrumbs
parseFunctionCreateResponse bytes = (parseMessage bytes) >>= getBreadcrumbs where
    getBreadcrumbs = decode . FunctionCreated.bc

parseGraphViewResponse :: ByteString -> Maybe GraphView.GraphView
parseGraphViewResponse bytes = GraphViewResponse.graph <$> (parseMessage bytes)

parseGetCodeResponse :: ByteString -> Maybe String
parseGetCodeResponse bytes = (uToString . GetCode.code) <$> (parseMessage bytes)

parseValueUpdate :: ByteString -> Maybe Value.Update
parseValueUpdate bytes = parseMessage bytes

parseAddNodeResponse :: ByteString -> Maybe Node
parseAddNodeResponse bytes = (parseMessage bytes) >>= getNode where
    getNode = decode . AddNode.node

-- TODO[MK]: REMOVE! This is needed for without-backend mode, not a production code
parseAddNodeFakeResponse :: ByteString -> Maybe Node
parseAddNodeFakeResponse bytes = (parseMessage bytes) >>= getNode where
    getNode = decode . AddNodeReq.node
