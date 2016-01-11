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
    (nodeE, newEmpireEnv) <- liftIO $ Empire.runEmpire currentEmpireEnv $ Server.withGraphLocation GraphCmd.addNode
        (request ^. AddNode.location)
        (Text.pack $ request ^. AddNode.expr)
        (request ^. AddNode.nodeMeta)
    case nodeE of
        Left err -> logger Logger.error $ Server.errorMessage ++ err
        Right node -> do
            Env.empireEnv .= newEmpireEnv
            let response = Response.Update request $ AddNode.Update node
            lift $ BusT $ Bus.send Flag.Enable $ Message.Message Topic.nodeUpdate $ toStrict $ Bin.encode response
            return () -- TODO: check Message.CorrelationID issue

handleRemoveNode :: ByteString -> StateT Env BusT ()
handleRemoveNode content = do
    let removeNodeRequest = Bin.decode . fromStrict $ content :: RemoveNode.Request
    logger Logger.info $ show removeNodeRequest

updateNodeMeta :: ByteString -> StateT Env BusT ()
updateNodeMeta content = do
    let request = Bin.decode . fromStrict $ content :: UpdateNodeMeta.Request
    logger Logger.info $ show request
    let response = Response.Update request $ UpdateNodeMeta.Update $ request ^. UpdateNodeMeta.nodeMeta
    void $ lift $ BusT $ Bus.send Flag.Enable $ Message.Message Topic.updateNodeMetaResponse $ toStrict $ Bin.encode response
