{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Prologue
import qualified Data.Binary                   as Bin
import qualified Data.ByteString               as ByteString
import qualified Data.ByteString.Char8         as Char8 (pack)
import           Data.ByteString.Lazy          (fromStrict, toStrict)
import qualified Flowbox.Config.Config         as Config
import qualified Flowbox.Bus.EndPoint          as EP
import qualified Flowbox.Bus.Bus               as Bus
import qualified Flowbox.Bus.Data.Flag         as Flag
import qualified Flowbox.Bus.Data.Message      as Message
import           Flowbox.Options.Applicative   hiding (info)
import qualified Flowbox.Options.Applicative   as Opt
import qualified Empire.API.Data.Node          as Node
import qualified Empire.API.Data.NodeMeta      as NodeMeta
import qualified Empire.API.Data.NodeMeta      as NodeMeta
import qualified Empire.API.Data.GraphLocation as GraphLocation
import qualified Empire.API.Data.Breadcrumb    as Breadcrumb
import qualified Empire.API.Graph.AddNode      as AddNode
import qualified Empire.API.Graph.RemoveNode   as RemoveNode
import qualified Empire.API.Topic              as Topic
import qualified Empire.API.Response           as Response


data Cmd = TestNotRecognizedRequest
         | TestBadTopic
         | TestAddNode
         | TestRemoveNode
         | TestNodeUpdate
         | TestAddProject
         | TestListProjects
         | TestAddLibrary
         | TestListLibraries
         deriving Show

parser :: Parser Cmd
parser = Opt.flag' TestNotRecognizedRequest (long "nrr")
     <|> Opt.flag' TestBadTopic             (long "bt")
     <|> Opt.flag' TestAddNode              (long "an")
     <|> Opt.flag' TestRemoveNode           (long "rn")
     <|> Opt.flag' TestNodeUpdate           (long "nu")
     <|> Opt.flag' TestAddProject           (long "ap")
     <|> Opt.flag' TestListProjects         (long "lp")
     <|> Opt.flag' TestAddLibrary           (long "al")
     <|> Opt.flag' TestListLibraries        (long "ll")

run :: Cmd -> IO ()
run cmd = case cmd of
    TestNotRecognizedRequest -> testNotRecognizedRequest
    TestBadTopic             -> testBadTopic
    TestAddNode              -> testAddNode
    TestRemoveNode           -> testRemoveNode
    TestNodeUpdate           -> testNodeUpdate
    TestAddProject           -> testAddProject
    TestListProjects         -> testListProjects
    TestAddLibrary           -> testAddLibrary
    TestListLibraries        -> testListLibraries


opts :: ParserInfo Cmd
opts = Opt.info (helper <*> parser)
                (Opt.fullDesc <> Opt.header "ver 0.1")

main = execParser opts >>= run

-- tests

gl :: Int -> Int -> GraphLocation.GraphLocation
gl pid lid = GraphLocation.GraphLocation pid lid Breadcrumb.Breadcrumb

testNotRecognizedRequest :: IO ()
testNotRecognizedRequest = do
    endPoints <- EP.clientFromConfig <$> Config.load
    Bus.runBus endPoints $ do
        Bus.send Flag.Enable $ Message.Message "empire.hello.request" (Char8.pack "basic string")
    return ()

testBadTopic :: IO ()
testBadTopic = do
    endPoints <- EP.clientFromConfig <$> Config.load
    Bus.runBus endPoints $ do
        Bus.send Flag.Enable $ Message.Message "any-hello" ByteString.empty
    return ()

testAddNode :: IO ()
testAddNode = do
    endPoints <- EP.clientFromConfig <$> Config.load
    let addNodeReq = AddNode.Request (gl 1 2) "expres" (NodeMeta.NodeMeta (1.2, 3.4)) 7
        content    = toStrict . Bin.encode $ addNodeReq
    Bus.runBus endPoints $ do
        Bus.send Flag.Enable $ Message.Message Topic.addNodeRequest content
    return ()

testRemoveNode :: IO ()
testRemoveNode = do
    endPoints <- EP.clientFromConfig <$> Config.load
    let removeNodeReq = RemoveNode.Request (gl 1 2) 3
        content       = toStrict . Bin.encode $ removeNodeReq
    Bus.runBus endPoints $ do
        Bus.send Flag.Enable $ Message.Message Topic.removeNodeRequest content
    return ()

testNodeUpdate :: IO ()
testNodeUpdate = do
    endPoints <- EP.clientFromConfig <$> Config.load
    Bus.runBus endPoints $ do
        let meta     = NodeMeta.NodeMeta (20.0, 30.0)
            request  = AddNode.Request (gl 0 0) "dupa123" meta 1235
            node     = Node.Node 123 "dupa123" mempty meta
            update   = AddNode.Update  node
            response = Response.Update request update
        Bus.send Flag.Enable $ Message.Message "empire.graph.node.add.update" $ toStrict $ Bin.encode response
    return ()

testAddProject :: IO ()
testAddProject = do
    endPoints <- EP.clientFromConfig <$> Config.load
    let addNodeReq = AddNode.Request (gl 1 2) "expres" (NodeMeta.NodeMeta (1.2, 3.4)) 7
        content    = toStrict . Bin.encode $ addNodeReq
    Bus.runBus endPoints $ do
        Bus.send Flag.Enable $ Message.Message Topic.addNodeRequest content
    return ()

testListProjects = return ()
testAddLibrary = return ()
testListLibraries = return ()
