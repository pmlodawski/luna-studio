{-# LANGUAGE OverloadedStrings #-}

module Empire.Server.Graph where

import           Prologue
import qualified Data.Binary                     as Bin
import           Control.Monad.State             (StateT)
import           Data.ByteString                 (ByteString)
import           Data.ByteString.Lazy            (fromStrict, toStrict)
import qualified Data.Text.Lazy                  as Text
import qualified Flowbox.System.Log.Logger       as Logger
import qualified Flowbox.Bus.Data.Flag           as Flag
import qualified Flowbox.Bus.Data.Message        as Message
import qualified Flowbox.Bus.Bus                 as Bus
import           Flowbox.Bus.BusT                (BusT (..))
import qualified Empire.Env                      as Env
import           Empire.Env                      (Env)
import qualified Empire.API.Graph.AddNode        as AddNode
import qualified Empire.API.Graph.RemoveNode     as RemoveNode
import qualified Empire.API.Graph.UpdateNodeMeta as UpdateNodeMeta
import qualified Empire.API.Graph.Connect        as Connect
import qualified Empire.API.Graph.Disconnect     as Disconnect
import qualified Empire.API.Graph.GetCode        as GetCode
import qualified Empire.API.Graph.GetGraph       as GetGraph
import qualified Empire.API.Response             as Response
import qualified Empire.API.Topic                as Topic
import qualified Empire.Commands.Graph           as GraphCmd
import qualified Empire.Empire                   as Empire
import qualified Empire.Server.Server            as Server

logger :: Logger.LoggerIO
logger = Logger.getLoggerIO $(Logger.moduleName)

handleAddNode :: ByteString -> StateT Env BusT ()
handleAddNode content = do
    let request = Bin.decode . fromStrict $ content :: AddNode.Request
    logger Logger.info $ show request
    currentEmpireEnv <- use Env.empireEnv
    logger Logger.info $ show currentEmpireEnv
    (result, newEmpireEnv) <- liftIO $ Empire.runEmpire currentEmpireEnv $ Server.withGraphLocation GraphCmd.addNode
        (request ^. AddNode.location)
        (Text.pack $ request ^. AddNode.expr)
        (request ^. AddNode.nodeMeta)
    case result of
        Left err -> logger Logger.error $ Server.errorMessage ++ err
        Right node -> do
            Env.empireEnv .= newEmpireEnv
            let response = Response.Update request $ AddNode.Update node
            lift $ BusT $ Bus.send Flag.Enable $ Message.Message Topic.nodeUpdate $ toStrict $ Bin.encode response
            return () -- TODO: check Message.CorrelationID issue

handleRemoveNode :: ByteString -> StateT Env BusT ()
handleRemoveNode content = do
    let request = Bin.decode . fromStrict $ content :: RemoveNode.Request
        nodeId  = request ^. RemoveNode.nodeId
    logger Logger.info $ show request
    currentEmpireEnv <- use Env.empireEnv
    logger Logger.info $ show currentEmpireEnv
    (result, newEmpireEnv) <- liftIO $ Empire.runEmpire currentEmpireEnv $ Server.withGraphLocation GraphCmd.removeNode
        (request ^. RemoveNode.location)
        nodeId
    case result of
        Left err -> logger Logger.error $ Server.errorMessage ++ err
        Right _ -> do
            Env.empireEnv .= newEmpireEnv
            let response = Response.Update request $ RemoveNode.Update nodeId
            lift $ BusT $ Bus.send Flag.Enable $ Message.Message Topic.nodeRemovedUpdate $ toStrict $ Bin.encode response
            return ()

handleUpdateNodeMeta :: ByteString -> StateT Env BusT ()
handleUpdateNodeMeta content = do
    let request = Bin.decode . fromStrict $ content :: UpdateNodeMeta.Request
        nodeMeta = request ^. UpdateNodeMeta.nodeMeta
    logger Logger.info $ show request
    currentEmpireEnv <- use Env.empireEnv
    logger Logger.info $ show currentEmpireEnv
    (result, newEmpireEnv) <- liftIO $ Empire.runEmpire currentEmpireEnv $ Server.withGraphLocation GraphCmd.updateNodeMeta
        (request ^. UpdateNodeMeta.location)
        (request ^. UpdateNodeMeta.nodeId)
        nodeMeta
    case result of
        Left err -> logger Logger.error $ Server.errorMessage ++ err
        Right _ -> do
            Env.empireEnv .= newEmpireEnv
            let response = Response.Update request $ UpdateNodeMeta.Update nodeMeta
            lift $ BusT $ Bus.send Flag.Enable $ Message.Message Topic.updateNodeMetaResponse $ toStrict $ Bin.encode response
            return ()

handleConnect :: ByteString -> StateT Env BusT ()
handleConnect content = do
    let request = Bin.decode . fromStrict $ content :: Connect.Request
    logger Logger.info $ show request
    currentEmpireEnv <- use Env.empireEnv
    logger Logger.info $ show currentEmpireEnv
    (result, newEmpireEnv) <- liftIO $ Empire.runEmpire currentEmpireEnv $ Server.withGraphLocation GraphCmd.connect
        (request ^. Connect.location)
        (request ^. Connect.src)
        (request ^. Connect.dst)
    case result of
        Left err -> logger Logger.error $ Server.errorMessage ++ err
        Right _ -> do
            Env.empireEnv .= newEmpireEnv
            let response = Response.Update request $ Response.Ok -- TODO: maybe update?
            lift $ BusT $ Bus.send Flag.Enable $ Message.Message Topic.connectResponse $ toStrict $ Bin.encode response
            return ()

handleDisconnect :: ByteString -> StateT Env BusT ()
handleDisconnect content = do
    let request = Bin.decode . fromStrict $ content :: Disconnect.Request
    logger Logger.info $ show request
    currentEmpireEnv <- use Env.empireEnv
    logger Logger.info $ show currentEmpireEnv
    (result, newEmpireEnv) <- liftIO $ Empire.runEmpire currentEmpireEnv $ Server.withGraphLocation GraphCmd.disconnect
        (request ^. Disconnect.location)
        (request ^. Disconnect.dst)
    case result of
        Left err -> logger Logger.error $ Server.errorMessage ++ err
        Right _ -> do
            Env.empireEnv .= newEmpireEnv
            let response = Response.Update request $ Response.Ok -- TODO: maybe update?
            lift $ BusT $ Bus.send Flag.Enable $ Message.Message Topic.disconnectResponse $ toStrict $ Bin.encode response
            return ()

handleGetCode :: ByteString -> StateT Env BusT ()
handleGetCode content = do
    let request = Bin.decode . fromStrict $ content :: GetCode.Request
    logger Logger.info $ show request
    currentEmpireEnv <- use Env.empireEnv
    logger Logger.info $ show currentEmpireEnv
    (result, newEmpireEnv) <- liftIO $ Empire.runEmpire currentEmpireEnv $ Server.withGraphLocation GraphCmd.getCode
        (request ^. GetCode.location)
    case result of
        Left err -> logger Logger.error $ Server.errorMessage ++ err
        Right code -> do
            Env.empireEnv .= newEmpireEnv
            let response = Response.Update request $ GetCode.Status code
            lift $ BusT $ Bus.send Flag.Enable $ Message.Message Topic.codeUpdate $ toStrict $ Bin.encode response
            return ()

handleGetGraph :: ByteString -> StateT Env BusT ()
handleGetGraph content = do
    let request = Bin.decode . fromStrict $ content :: GetGraph.Request
    logger Logger.info $ show request
    currentEmpireEnv <- use Env.empireEnv
    logger Logger.info $ show currentEmpireEnv
    (result, newEmpireEnv) <- liftIO $ Empire.runEmpire currentEmpireEnv $ Server.withGraphLocation GraphCmd.getGraph
        (request ^. GetGraph.location)
    case result of
        Left err -> logger Logger.error $ Server.errorMessage ++ err
        Right graph -> do
            Env.empireEnv .= newEmpireEnv
            let response = Response.Update request $ GetGraph.Status graph
            lift $ BusT $ Bus.send Flag.Enable $ Message.Message Topic.graphUpdate $ toStrict $ Bin.encode response
            return ()
