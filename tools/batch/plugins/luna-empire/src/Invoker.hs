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
import qualified Empire.API.Data.Node             as Node
import qualified Empire.API.Data.NodeMeta         as NodeMeta
import qualified Empire.API.Data.NodeMeta         as NodeMeta
import qualified Empire.API.Data.GraphLocation    as GraphLocation
import qualified Empire.API.Data.Breadcrumb       as Breadcrumb
import qualified Empire.API.Graph.AddNode         as AddNode
import qualified Empire.API.Graph.RemoveNode      as RemoveNode
import qualified Empire.API.Graph.UpdateNodeMeta  as UpdateNodeMeta
import qualified Empire.API.Graph.GetProgram      as GetProgram
import qualified Empire.API.Project.CreateProject as CreateProject
import qualified Empire.API.Project.ListProjects  as ListProjects
import qualified Empire.API.Library.CreateLibrary as CreateLibrary
import qualified Empire.API.Library.ListLibraries as ListLibraries
import qualified Empire.API.Topic                 as Topic
import qualified Empire.API.Update                as Update

gl :: Int -> Int -> GraphLocation.GraphLocation
gl pid lid = GraphLocation.GraphLocation pid lid Breadcrumb.Breadcrumb

patterns :: Docopt
patterns = [docoptFile|src/InvokerUsage.txt|]

getArgOrExit = getArgOrExitWith patterns

main :: IO ()
main = do
    args <- parseArgsOrExit patterns =<< getArgs
    endPoints <- EP.clientFromConfig <$> Config.load
    when (args `isPresent` (command "addNode")) $ do
        pid    <- args `getArgOrExit` (argument "pid")
        lid    <- args `getArgOrExit` (argument "lid")
        expr   <- args `getArgOrExit` (argument "expression")
        x      <- args `getArgOrExit` (argument "x")
        y      <- args `getArgOrExit` (argument "y")
        tag    <- args `getArgOrExit` (argument "tag")
        addNode endPoints (read pid) (read lid) expr (read x) (read y) (read tag)
    when (args `isPresent` (command "removeNode")) $ do
        pid    <- args `getArgOrExit` (argument "pid")
        lid    <- args `getArgOrExit` (argument "lid")
        nodeId <- args `getArgOrExit` (argument "nodeId")
        removeNode endPoints (read pid) (read lid) (read nodeId)
    when (args `isPresent` (command "updateNodeMeta")) $ do
        pid    <- args `getArgOrExit` (argument "pid")
        lid    <- args `getArgOrExit` (argument "lid")
        nodeId <- args `getArgOrExit` (argument "nodeId")
        x      <- args `getArgOrExit` (argument "x")
        y      <- args `getArgOrExit` (argument "y")
        updateNodeMeta endPoints (read pid) (read lid) (read nodeId) (read x) (read y)
    when (args `isPresent` (command "getProgram")) $ do
        pid    <- args `getArgOrExit` (argument "pid")
        lid    <- args `getArgOrExit` (argument "lid")
        getProgram endPoints (read pid) (read lid)
    when (args `isPresent` (command "createLibrary")) $ do
        pid    <- args `getArgOrExit` (argument "pid")
        path   <- args `getArgOrExit` (argument "path")
        let name = args `getArg` (longOption "name")
        createLibrary endPoints (read pid) name path
    when (args `isPresent` (command "libraries")) $ do
        pid    <- args `getArgOrExit` (argument "pid")
        listLibraries endPoints $ read pid
    when (args `isPresent` (command "createProject")) $ do
        path   <- args `getArgOrExit` (argument "path")
        let name = args `getArg` (longOption "name")
        createProject endPoints Nothing path
    when (args `isPresent` (command "projects")) $ do
        listProjects endPoints

addNode :: EP.BusEndPoints -> Int -> Int -> String -> Double -> Double -> Int -> IO ()
addNode endPoints pid lid expression x y tag = do
    let content = toStrict . Bin.encode $ AddNode.Request (gl pid lid) expression (NodeMeta.NodeMeta (x, y)) tag
    void $ Bus.runBus endPoints $ Bus.send Flag.Enable $ Message.Message Topic.addNodeRequest content

removeNode :: EP.BusEndPoints -> Int -> Int -> Int -> IO ()
removeNode endPoints pid lid nodeId = do
    let content = toStrict . Bin.encode $ RemoveNode.Request (gl pid lid) nodeId
    void $ Bus.runBus endPoints $ Bus.send Flag.Enable $ Message.Message Topic.removeNodeRequest content

updateNodeMeta :: EP.BusEndPoints -> Int -> Int -> Int -> Double -> Double -> IO ()
updateNodeMeta endPoints pid lid nodeId x y = do
    let content = toStrict . Bin.encode $ UpdateNodeMeta.Request (gl pid lid) nodeId (NodeMeta.NodeMeta (x, y))
    void $ Bus.runBus endPoints $ Bus.send Flag.Enable $ Message.Message Topic.updateNodeMetaRequest content

getProgram :: EP.BusEndPoints -> Int -> Int -> IO ()
getProgram endPoints pid lid = do
    let content = toStrict . Bin.encode $ GetProgram.Request (gl pid lid)
    void $ Bus.runBus endPoints $ Bus.send Flag.Enable $ Message.Message Topic.programRequest content

createProject :: EP.BusEndPoints -> Maybe String -> String -> IO ()
createProject endPoints name path = do
    let content = toStrict . Bin.encode $ CreateProject.Request name path
    void $ Bus.runBus endPoints $ Bus.send Flag.Enable $ Message.Message Topic.createProjectRequest content

listProjects :: EP.BusEndPoints -> IO ()
listProjects endPoints = do
    let content = toStrict . Bin.encode $ ListProjects.Request
    void $ Bus.runBus endPoints $ Bus.send Flag.Enable $ Message.Message Topic.listProjectsRequest content

createLibrary :: EP.BusEndPoints -> Int -> Maybe String -> String -> IO ()
createLibrary endPoints pid name path = do
    let content = toStrict . Bin.encode $ CreateLibrary.Request pid name path
    void $ Bus.runBus endPoints $ Bus.send Flag.Enable $ Message.Message Topic.createLibraryRequest content

listLibraries :: EP.BusEndPoints -> Int -> IO ()
listLibraries endPoints pid = do
    let content = toStrict . Bin.encode $ ListLibraries.Request pid
    void $ Bus.runBus endPoints $ Bus.send Flag.Enable $ Message.Message Topic.listLibrariesRequest content
