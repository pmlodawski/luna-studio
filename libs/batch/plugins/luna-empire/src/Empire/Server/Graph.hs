{-# LANGUAGE OverloadedStrings #-}

module Empire.Server.Graph where

import           Prologue
import qualified Data.Binary                     as Bin
import           Control.Monad.State             (StateT, get, put)
import           Data.Map.Strict                 (Map)
import           Data.ByteString                 (ByteString)
import           Data.ByteString.Char8           (unpack)
import           Data.ByteString.Lazy            (fromStrict, toStrict)
import           Data.Text.Lazy                  (Text)
import qualified Data.Text.Lazy                  as Text
import           Flowbox.System.Log.Logger
import qualified Flowbox.Bus.Data.Flag           as Flag
import qualified Flowbox.Bus.Data.Message        as Message
import qualified Flowbox.Bus.Bus                 as Bus
import           Flowbox.Bus.BusT                (BusT (..))
import qualified Flowbox.Bus.BusT                as Bus
import qualified Empire.Env                      as Env
import           Empire.Env                      (Env)
import qualified Empire.API.Data.Node            as Node
import           Empire.API.Data.Node            (Node)
import qualified Empire.API.Data.NodeMeta        as NodeMeta
import           Empire.API.Data.NodeMeta        (NodeMeta)
import           Empire.API.Data.GraphLocation   (GraphLocation)
import qualified Empire.API.Data.GraphLocation   as GraphLocation
import           Empire.API.Data.Library         (LibraryId)
import           Empire.API.Data.Project         (ProjectId)
import qualified Empire.API.Graph.AddNode        as AddNode
import qualified Empire.API.Graph.RemoveNode     as RemoveNode
import qualified Empire.API.Graph.UpdateNodeMeta as UpdateNodeMeta
import qualified Empire.API.Response             as Response
import qualified Empire.Commands.Graph           as GraphCmd
import           Empire.Data.AST                 (AST)
import qualified Empire.Empire                   as Empire
import           Empire.Empire                   (Empire)

logger :: LoggerIO
logger = getLoggerIO $moduleName

withGraphLocation :: (ProjectId -> LibraryId -> a) -> GraphLocation -> a
withGraphLocation f graphLocation = f (graphLocation ^. GraphLocation.projectId)
                                      (graphLocation ^. GraphLocation.libraryId)

addNode :: GraphLocation -> Text -> NodeMeta -> Empire Node
addNode graphLocation expr meta = do
    node <- withGraphLocation GraphCmd.addNode graphLocation expr meta
    return node

handleAddNode :: ByteString -> StateT Env BusT ()
handleAddNode content = do
    let request = Bin.decode . fromStrict $ content :: AddNode.Request
    logger info $ show request
    currentEmpireEnv <- use Env.empireEnv
    logger info $ show currentEmpireEnv
    (nodeE, newEmpireEnv) <- liftIO $ Empire.runEmpire currentEmpireEnv $ addNode
        (request ^. AddNode.location)
        (Text.pack $ request ^. AddNode.expr)
        (request ^. AddNode.nodeMeta)
    Env.empireEnv .= newEmpireEnv
    case nodeE of
        Left err -> logger info $ "Error processing request: " ++ show err
        Right node -> do
            let update   = AddNode.Update  node
                response = Response.Update request update
            lift $ BusT $ Bus.send Flag.Enable $ Message.Message "empire.graph.node.add.update" $ toStrict $ Bin.encode response
            return ()

handleRemoveNode :: ByteString -> StateT Env BusT ()
handleRemoveNode content = do
    let removeNodeRequest = Bin.decode . fromStrict $ content :: RemoveNode.Request
    logger info $ show removeNodeRequest

updateNodeMeta :: ByteString -> StateT Env BusT ()
updateNodeMeta content = do
    let request = Bin.decode . fromStrict $ content :: UpdateNodeMeta.Request
    logger info $ show request
    let response = Response.Update request $ UpdateNodeMeta.Update $ request ^. UpdateNodeMeta.nodeMeta
    void $ lift $ BusT $ Bus.send Flag.Enable $ Message.Message "empire.graph.node.updateMeta.update" $ toStrict $ Bin.encode response
