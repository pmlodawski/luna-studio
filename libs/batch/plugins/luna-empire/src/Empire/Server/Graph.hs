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
import qualified Empire.API.Graph.GetProgram     as GetProgram
import qualified Empire.API.Graph.CodeUpdate     as CodeUpdate
import qualified Empire.API.Response             as Response
import qualified Empire.API.Topic                as Topic
import           Empire.API.Data.GraphLocation  (GraphLocation)
import qualified Empire.Commands.Graph           as GraphCmd
import qualified Empire.Empire                   as Empire
import qualified Empire.Server.Server            as Server

logger :: Logger.LoggerIO
logger = Logger.getLoggerIO $(Logger.moduleName)


notifyCodeUpdate :: GraphLocation -> StateT Env BusT ()
notifyCodeUpdate location = do
    currentEmpireEnv <- use Env.empireEnv
    (resultCode, _) <- liftIO $ Empire.runEmpire currentEmpireEnv $ Server.withGraphLocation GraphCmd.getCode
        location
    case resultCode of
        Left err -> logger Logger.error $ Server.errorMessage ++ err
        Right code -> do
            let response = CodeUpdate.Update location $ Text.pack code
            void . lift $ BusT $ Bus.send Flag.Enable $ Message.Message Topic.codeUpdate $ toStrict $ Bin.encode response

handleAddNode :: ByteString -> StateT Env BusT ()
handleAddNode content = do
    let request  = Bin.decode . fromStrict $ content :: AddNode.Request
        location = request ^. AddNode.location
    currentEmpireEnv <- use Env.empireEnv
    logger Logger.info $ show request
    logger Logger.info $ show currentEmpireEnv
    (result, newEmpireEnv) <- liftIO $ Empire.runEmpire currentEmpireEnv $ Server.withGraphLocation GraphCmd.addNode
        location
        (Text.pack $ request ^. AddNode.expr)
        (request ^. AddNode.nodeMeta)
    case result of
        Left err -> logger Logger.error $ Server.errorMessage ++ err
        Right node -> do
            Env.empireEnv .= newEmpireEnv
            let response = Response.Update request $ AddNode.Update node
            lift $ BusT $ Bus.send Flag.Enable $ Message.Message Topic.addNodeUpdate $ toStrict $ Bin.encode response
            notifyCodeUpdate location

handleRemoveNode :: ByteString -> StateT Env BusT ()
handleRemoveNode content = do
    let request = Bin.decode . fromStrict $ content :: RemoveNode.Request
        location = request ^. RemoveNode.location
    currentEmpireEnv <- use Env.empireEnv
    logger Logger.info $ show request
    logger Logger.info $ show currentEmpireEnv
    (result, newEmpireEnv) <- liftIO $ Empire.runEmpire currentEmpireEnv $ Server.withGraphLocation GraphCmd.removeNode
        location
        (request ^. RemoveNode.nodeId)
    case result of
        Left err -> logger Logger.error $ Server.errorMessage ++ err
        Right _ -> do
            Env.empireEnv .= newEmpireEnv
            let response = Response.Update request $ Response.Ok
            lift $ BusT $ Bus.send Flag.Enable $ Message.Message Topic.removeNodeUpdate $ toStrict $ Bin.encode response
            notifyCodeUpdate location

handleUpdateNodeMeta :: ByteString -> StateT Env BusT ()
handleUpdateNodeMeta content = do
    let request = Bin.decode . fromStrict $ content :: UpdateNodeMeta.Request
        nodeMeta = request ^. UpdateNodeMeta.nodeMeta
    currentEmpireEnv <- use Env.empireEnv
    logger Logger.info $ show request
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
            lift $ BusT $ Bus.send Flag.Enable $ Message.Message Topic.updateNodeMetaUpdate $ toStrict $ Bin.encode response
            return ()

handleConnect :: ByteString -> StateT Env BusT ()
handleConnect content = do
    let request = Bin.decode . fromStrict $ content :: Connect.Request
        location = request ^. Connect.location
    currentEmpireEnv <- use Env.empireEnv
    logger Logger.info $ show request
    logger Logger.info $ show currentEmpireEnv
    (result, newEmpireEnv) <- liftIO $ Empire.runEmpire currentEmpireEnv $ Server.withGraphLocation GraphCmd.connect
        location
        (request ^. Connect.src)
        (request ^. Connect.dst)
    case result of
        Left err -> logger Logger.error $ Server.errorMessage ++ err
        Right _ -> do
            Env.empireEnv .= newEmpireEnv
            let response = Response.Update request $ Response.Ok
            lift $ BusT $ Bus.send Flag.Enable $ Message.Message Topic.connectUpdate $ toStrict $ Bin.encode response
            notifyCodeUpdate location

handleDisconnect :: ByteString -> StateT Env BusT ()
handleDisconnect content = do
    let request = Bin.decode . fromStrict $ content :: Disconnect.Request
        location = request ^. Disconnect.location
    currentEmpireEnv <- use Env.empireEnv
    logger Logger.info $ show request
    logger Logger.info $ show currentEmpireEnv
    (result, newEmpireEnv) <- liftIO $ Empire.runEmpire currentEmpireEnv $ Server.withGraphLocation GraphCmd.disconnect
        location
        (request ^. Disconnect.dst)
    case result of
        Left err -> logger Logger.error $ Server.errorMessage ++ err
        Right _ -> do
            Env.empireEnv .= newEmpireEnv
            let response = Response.Update request $ Response.Ok
            lift $ BusT $ Bus.send Flag.Enable $ Message.Message Topic.disconnectUpdate $ toStrict $ Bin.encode response
            notifyCodeUpdate location

handleGetProgram :: ByteString -> StateT Env BusT ()
handleGetProgram content = do
    let request = Bin.decode . fromStrict $ content :: GetProgram.Request
    currentEmpireEnv <- use Env.empireEnv
    logger Logger.info $ show request
    logger Logger.info $ show currentEmpireEnv
    (resultGraph, _) <- liftIO $ Empire.runEmpire currentEmpireEnv $ Server.withGraphLocation GraphCmd.getGraph
        (request ^. GetProgram.location)
    (resultCode, _) <- liftIO $ Empire.runEmpire currentEmpireEnv $ Server.withGraphLocation GraphCmd.getCode
        (request ^. GetProgram.location)
    case (resultGraph, resultCode) of
        (Left err, _) -> logger Logger.error $ Server.errorMessage ++ err
        (_, Left err) -> logger Logger.error $ Server.errorMessage ++ err
        (Right graph, Right code) -> do
            let response = Response.Update request $ GetProgram.Status graph (Text.pack code)
            void . lift $ BusT $ Bus.send Flag.Enable $ Message.Message Topic.programStatus $ toStrict $ Bin.encode response
