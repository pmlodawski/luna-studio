{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Prologue
import qualified Data.Binary                 as Bin
import qualified Data.ByteString             as ByteString
import qualified Data.ByteString.Char8       as Char8 (pack)
import           Data.ByteString.Lazy        (fromStrict, toStrict)

import qualified Flowbox.Config.Config       as Config
import qualified Flowbox.Bus.EndPoint        as EP
import qualified Flowbox.Bus.Bus             as Bus
import qualified Flowbox.Bus.Data.Flag       as Flag
import qualified Flowbox.Bus.Data.Message    as Message
import           Flowbox.Options.Applicative hiding (info)
import qualified Flowbox.Options.Applicative as Opt

import qualified Empire.API.Data.Node as Node
import qualified Empire.API.Data.NodeMeta as NodeMeta
import qualified Empire.API.Topic            as Topic
import qualified Empire.API.Graph.AddNode    as AddNode
import qualified Empire.API.Graph.RemoveNode as RemoveNode
import qualified Empire.API.Data.NodeMeta    as NodeMeta
import qualified Empire.API.Response         as Response


data Cmd = TestBasicString
         | TestBadTopic
         | TestAddNode
         | TestRemoveNode
         | Test5
         deriving Show

parser :: Parser Cmd
parser = Opt.flag' TestBasicString (short 'S')
     <|> Opt.flag' TestBadTopic (short 'B')
     <|> Opt.flag' TestAddNode (short 'a')
     <|> Opt.flag' TestRemoveNode (short 'r')
     <|> Opt.flag' Test5 (short '5')

run :: Cmd -> IO ()
run cmd = case cmd of
    TestBasicString -> testBasicString
    TestBadTopic    -> testBadTopic
    TestAddNode     -> testAddNode
    TestRemoveNode  -> testRemoveNode
    Test5           -> test5

opts :: ParserInfo Cmd
opts = Opt.info (helper <*> parser)
                (Opt.fullDesc <> Opt.header "ver 0.1")

main = execParser opts >>= run

-- tests

testBasicString :: IO ()
testBasicString = do
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
    let addNodeReq = AddNode.Request 1 2 "expres" (NodeMeta.NodeMeta (1.2, 3.4)) 7
        content    = toStrict . Bin.encode $ addNodeReq
    Bus.runBus endPoints $ do
        Bus.send Flag.Enable $ Message.Message Topic.addNodeRequest content
    return ()

testRemoveNode :: IO ()
testRemoveNode = do
    endPoints <- EP.clientFromConfig <$> Config.load
    let removeNodeReq = RemoveNode.Request 1 2 3
        content       = toStrict . Bin.encode $ removeNodeReq
    Bus.runBus endPoints $ do
        Bus.send Flag.Enable $ Message.Message Topic.removeNodeRequest content
    return ()

test5 :: IO ()
test5 = do
    endPoints <- EP.clientFromConfig <$> Config.load
    Bus.runBus endPoints $ do
      let meta     = NodeMeta.NodeMeta (20.0, 30.0)
          request  = AddNode.Request 0 0 "dupa123" meta 1235
          node     = Node.Node 123 "dupa123" mempty meta
          update   = AddNode.Update  node
          response = Response.Update request update
      Bus.send Flag.Enable $ Message.Message "empire.graph.node.add.update" $ toStrict $ Bin.encode response
    return ()
