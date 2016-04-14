{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Main where

import           Prologue                         hiding (argument)
import           System.Environment               (getArgs)
import           System.Console.Docopt
import qualified Data.Binary                      as Bin
import qualified Data.ByteString                  as ByteString
import qualified Data.ByteString.Char8            as Char8 (pack)
import           Data.ByteString.Lazy             (fromStrict, toStrict)
import qualified Flowbox.Config.Config            as Config
import qualified Flowbox.Bus.EndPoint             as EP
import qualified Flowbox.Bus.Bus                  as Bus
import qualified Flowbox.Bus.Data.Flag            as Flag
import qualified Flowbox.Bus.Data.Message         as Message
import           Flowbox.Options.Applicative      (short, long, help, metavar)
import qualified Flowbox.Options.Applicative      as Opt
import           Empire.API.Data.Node             (NodeId)
import qualified Empire.API.Data.Node             as Node
import qualified Empire.API.Data.NodeMeta         as NodeMeta
import qualified Empire.API.Data.NodeMeta         as NodeMeta
import           Empire.API.Data.GraphLocation    (GraphLocation)
import qualified Empire.API.Data.GraphLocation    as GraphLocation
import           Empire.API.Data.Port             (OutPort, InPort)
import qualified Empire.API.Data.Breadcrumb       as Breadcrumb
import           Empire.API.Data.Project          (ProjectId)
import qualified Empire.API.Graph.AddNode         as AddNode
import qualified Empire.API.Graph.RemoveNode      as RemoveNode
import qualified Empire.API.Graph.Connect         as Connect
import qualified Empire.API.Graph.Disconnect      as Disconnect
import qualified Empire.API.Graph.UpdateNodeMeta  as UpdateNodeMeta
import qualified Empire.API.Graph.GetProgram      as GetProgram
import qualified Empire.API.Graph.DumpGraphViz    as DumpGraphViz
import qualified Empire.API.Graph.TypeCheck       as TypeCheck
import qualified Empire.API.Project.CreateProject as CreateProject
import qualified Empire.API.Project.ListProjects  as ListProjects
import qualified Empire.API.Library.CreateLibrary as CreateLibrary
import qualified Empire.API.Library.ListLibraries as ListLibraries
import qualified Empire.API.Topic                 as Topic
import qualified Empire.API.Update                as Update


toGraphLocation :: String -> String -> GraphLocation
toGraphLocation pid lid = GraphLocation.GraphLocation (read pid) (read lid) (Breadcrumb.Breadcrumb [])

patterns :: Docopt
patterns = [docoptFile|src/InvokerUsage.txt|]

getArgOrExit = getArgOrExitWith patterns

main :: IO ()
main = do
    args <- parseArgsOrExit patterns =<< getArgs
    endPoints <- EP.clientFromConfig <$> Config.load
    when (args `isPresent` (command "addNode")) $ do
        pid       <- args `getArgOrExit` (argument "pid")
        lid       <- args `getArgOrExit` (argument "lid")
        expr      <- args `getArgOrExit` (argument "expression")
        x         <- args `getArgOrExit` (argument "x")
        y         <- args `getArgOrExit` (argument "y")
        tag       <- args `getArgOrExit` (argument "tag")
        addNode endPoints (toGraphLocation pid lid) expr (read x) (read y) (read tag)
    when (args `isPresent` (command "removeNode")) $ do
        pid       <- args `getArgOrExit` (argument "pid")
        lid       <- args `getArgOrExit` (argument "lid")
        nodeId    <- args `getArgOrExit` (argument "nodeId")
        removeNode endPoints (toGraphLocation pid lid) (read nodeId)
    when (args `isPresent` (command "updateNodeMeta")) $ do
        pid       <- args `getArgOrExit` (argument "pid")
        lid       <- args `getArgOrExit` (argument "lid")
        nodeId    <- args `getArgOrExit` (argument "nodeId")
        x         <- args `getArgOrExit` (argument "x")
        y         <- args `getArgOrExit` (argument "y")
        updateNodeMeta endPoints (toGraphLocation pid lid) (read nodeId) (read x) (read y)
    when (args `isPresent` (command "connect")) $ do
        pid       <- args `getArgOrExit` (argument "pid")
        lid       <- args `getArgOrExit` (argument "lid")
        srcNodeId <- args `getArgOrExit` (argument "srcNodeId")
        outPort   <- args `getArgOrExit` (argument "outPort")
        dstNodeId <- args `getArgOrExit` (argument "dstNodeId")
        inPort    <- args `getArgOrExit` (argument "inPort")
        connect endPoints (toGraphLocation pid lid) (read srcNodeId) (read outPort) (read dstNodeId) (read inPort)
    when (args `isPresent` (command "disconnect")) $ do
        pid       <- args `getArgOrExit` (argument "pid")
        lid       <- args `getArgOrExit` (argument "lid")
        dstNodeId <- args `getArgOrExit` (argument "dstNodeId")
        inPort    <- args `getArgOrExit` (argument "inPort")
        disconnect endPoints (toGraphLocation pid lid) (read dstNodeId) (read inPort)
    when (args `isPresent` (command "getProgram")) $ do
        pid       <- args `getArgOrExit` (argument "pid")
        lid       <- args `getArgOrExit` (argument "lid")
        getProgram endPoints (toGraphLocation pid lid)
    when (args `isPresent` (command "createProject")) $ do
        path      <- args `getArgOrExit` (argument "path")
        let name   = args `getArg`       (argument "name")
        createProject endPoints name path
    when (args `isPresent` (command "createLibrary")) $ do
        pid       <- args `getArgOrExit` (argument "pid")
        path      <- args `getArgOrExit` (argument "path")
        let name   = args `getArg`       (argument "name")
        createLibrary endPoints (read pid) name path
    when (args `isPresent` (command "projects")) $ do
        listProjects endPoints
    when (args `isPresent` (command "libraries")) $ do
        pid       <- args `getArgOrExit` (argument "pid")
        listLibraries endPoints $ read pid
    when (args `isPresent` (command "dump")) $ do
        environmentDump endPoints
    when (args `isPresent` (command "graphviz")) $ do
        pid       <- args `getArgOrExit` (argument "pid")
        lid       <- args `getArgOrExit` (argument "lid")
        environmentDumpGraphviz endPoints $ toGraphLocation pid lid
    when (args `isPresent` (command "typecheck")) $ do
        pid       <- args `getArgOrExit` (argument "pid")
        lid       <- args `getArgOrExit` (argument "lid")
        typecheck endPoints $ toGraphLocation pid lid

addNode :: EP.BusEndPoints -> GraphLocation -> String -> Double -> Double -> Int -> IO ()
addNode endPoints graphLocation expression x y tag = do
    let content = toStrict . Bin.encode $ AddNode.Request graphLocation (AddNode.ExpressionNode expression) (NodeMeta.NodeMeta (x, y)) Nothing tag
    void $ Bus.runBus endPoints $ Bus.send Flag.Enable $ Message.Message Topic.addNodeRequest content

removeNode :: EP.BusEndPoints -> GraphLocation -> NodeId -> IO ()
removeNode endPoints graphLocation nodeId = do
    let content = toStrict . Bin.encode $ RemoveNode.Request graphLocation [nodeId]
    void $ Bus.runBus endPoints $ Bus.send Flag.Enable $ Message.Message Topic.removeNodeRequest content

updateNodeMeta :: EP.BusEndPoints -> GraphLocation -> NodeId -> Double -> Double -> IO ()
updateNodeMeta endPoints graphLocation nodeId x y = do
    let content = toStrict . Bin.encode $ UpdateNodeMeta.Request graphLocation nodeId (NodeMeta.NodeMeta (x, y))
    void $ Bus.runBus endPoints $ Bus.send Flag.Enable $ Message.Message Topic.updateNodeMetaRequest content

connect :: EP.BusEndPoints -> GraphLocation -> NodeId -> OutPort -> NodeId -> InPort -> IO ()
connect endPoints graphLocation srcNodeId outPort dstNodeId inPort = do
    let content = toStrict . Bin.encode $ Connect.Request graphLocation srcNodeId outPort dstNodeId inPort
    void $ Bus.runBus endPoints $ Bus.send Flag.Enable $ Message.Message Topic.connectRequest content

disconnect :: EP.BusEndPoints -> GraphLocation -> NodeId -> InPort -> IO ()
disconnect endPoints graphLocation  dstNodeId inPort = do
    let content = toStrict . Bin.encode $ Disconnect.Request graphLocation dstNodeId inPort
    void $ Bus.runBus endPoints $ Bus.send Flag.Enable $ Message.Message Topic.disconnectRequest content

getProgram :: EP.BusEndPoints -> GraphLocation -> IO ()
getProgram endPoints graphLocation = do
    let content = toStrict . Bin.encode $ GetProgram.Request graphLocation
    void $ Bus.runBus endPoints $ Bus.send Flag.Enable $ Message.Message Topic.programRequest content

createProject :: EP.BusEndPoints -> Maybe String -> String -> IO ()
createProject endPoints name path = do
    let content = toStrict . Bin.encode $ CreateProject.Request name path
    void $ Bus.runBus endPoints $ Bus.send Flag.Enable $ Message.Message Topic.createProjectRequest content

listProjects :: EP.BusEndPoints -> IO ()
listProjects endPoints = do
    let content = toStrict . Bin.encode $ ListProjects.Request
    void $ Bus.runBus endPoints $ Bus.send Flag.Enable $ Message.Message Topic.listProjectsRequest content

createLibrary :: EP.BusEndPoints -> ProjectId -> Maybe String -> String -> IO ()
createLibrary endPoints pid name path = do
    let content = toStrict . Bin.encode $ CreateLibrary.Request pid name path
    void $ Bus.runBus endPoints $ Bus.send Flag.Enable $ Message.Message Topic.createLibraryRequest content

listLibraries :: EP.BusEndPoints -> ProjectId -> IO ()
listLibraries endPoints pid = do
    let content = toStrict . Bin.encode $ ListLibraries.Request pid
    void $ Bus.runBus endPoints $ Bus.send Flag.Enable $ Message.Message Topic.listLibrariesRequest content

environmentDump :: EP.BusEndPoints -> IO ()
environmentDump endPoints = do
    let content = toStrict . Bin.encode $ ("" :: String)
    void $ Bus.runBus endPoints $ Bus.send Flag.Enable $ Message.Message Topic.logEnvDebug content

environmentDumpGraphviz :: EP.BusEndPoints -> GraphLocation -> IO ()
environmentDumpGraphviz endPoints loc = do
    let content = toStrict . Bin.encode $ DumpGraphViz.Request loc
    void $ Bus.runBus endPoints $ Bus.send Flag.Enable $ Message.Message Topic.logEnvDebugGraphViz content

typecheck :: EP.BusEndPoints -> GraphLocation -> IO ()
typecheck endPoints loc = do
    let content = toStrict . Bin.encode $ TypeCheck.Request loc
    void $ Bus.runBus endPoints $ Bus.send Flag.Enable $ Message.Message Topic.typecheck content
