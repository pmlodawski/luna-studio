module BatchConnector.Updates where

import           Utils.PreludePlus
import           Data.ByteString.Lazy
import qualified Data.Sequence              as Seq
import           Data.Text.Lazy             (Text)
import           Text.ProtocolBuffers
import           Text.ProtocolBuffers.Basic (uToString)

import           Batch.Project
import           Batch.Library
import           Batch.Breadcrumbs
import           Batch.Value
import           Object.Node
import           Object.Object              (PortId(..))

import           BatchConnector.Conversion  (decode)

import qualified Generated.Proto.Dep.Graphview.GraphView                      as GraphView
import qualified Generated.Proto.ProjectManager.Project.List.Status           as ProjectsList
import qualified Generated.Proto.ProjectManager.Project.Create.Update         as ProjectCreated
import qualified Generated.Proto.ProjectManager.Project.Library.List.Status   as LibsList
import qualified Generated.Proto.ProjectManager.Project.Library.Create.Update as LibCreated
import qualified Generated.Proto.ProjectManager.Project.Library.AST.Function.Add.Update             as FunctionCreated
import qualified Generated.Proto.ProjectManager.Project.Library.AST.Function.Graph.Get.Status       as GraphViewResponse
import qualified Generated.Proto.ProjectManager.Project.Library.AST.Code.Get.Status                 as GetCode
import qualified Generated.Proto.Interpreter.Interpreter.Value.Update                               as Value
import qualified Generated.Proto.Interpreter.CallPointPath                                          as CallPointPath
import qualified Generated.Proto.Interpreter.CallPoint                                              as CallPoint
import qualified Generated.Proto.Mode.ModeValue                                                     as ModeValue
import qualified Generated.Proto.ProjectManager.Project.Library.AST.Function.Graph.Node.Add.Update  as AddNode
import qualified Generated.Proto.ProjectManager.Project.Library.AST.Function.Graph.Node.Add.Request as AddNodeReq
import qualified Generated.Proto.Interpreter.Interpreter.GetProjectID.Status                        as GetProjectId

parseMessage :: (Wire m, ReflectDescriptor m) => ByteString -> Maybe m
parseMessage bytes = case messageGet bytes of
    Left  _        -> Nothing
    Right (msg, _) -> Just msg

parseProjectsListResponse :: ByteString -> Maybe [Project]
parseProjectsListResponse bytes = (parseMessage bytes) >>= pluckProjects where
    pluckProjects  = decode . ProjectsList.projects

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

parseGraphViewResponse :: ByteString -> Maybe ([Node], [(PortRef, PortRef)])
parseGraphViewResponse bytes = do
    parsed    <- parseMessage bytes
    let graph =  GraphViewResponse.graph parsed
    nodes <- decode $ GraphView.nodes graph
    edges <- decode $ GraphView.edges graph
    return (nodes, edges)

parseGetCodeResponse :: ByteString -> Maybe Text
parseGetCodeResponse bytes = (parseMessage bytes) >>= (decode . GetCode.code)

parseValueUpdate :: ByteString -> Maybe (Int, Value)
parseValueUpdate bytes = do
    response    <- parseMessage bytes
    nodeId      <- case (toList $ CallPointPath.calls $ Value.callPointPath response) of
        []              -> Nothing
        (callpoint : _) -> decode $ CallPoint.nodeID callpoint
    modeValue   <- case (toList $ Value.modeValue response) of
        []        -> Nothing
        (val : _) -> Just val
    svalue      <- ModeValue.value modeValue
    decodedValue <- decode svalue
    return (nodeId, decodedValue)

parseAddNodeResponse :: ByteString -> Maybe Node
parseAddNodeResponse bytes = (parseMessage bytes) >>= getNode where
    getNode = decode . AddNode.node

parseProjectIdStatus :: ByteString -> Maybe (Maybe Int32)
parseProjectIdStatus bytes = GetProjectId.projectID <$> parseMessage bytes

-- TODO[MK]: REMOVE! This is needed for without-backend mode, not a production code
parseAddNodeFakeResponse :: ByteString -> Maybe Node
parseAddNodeFakeResponse bytes = (parseMessage bytes) >>= getNode where
    getNode = decode . AddNodeReq.node
