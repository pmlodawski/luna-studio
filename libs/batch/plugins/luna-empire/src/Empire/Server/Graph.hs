{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns      #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE RankNTypes  #-}

module Empire.Server.Graph where

import           Control.Monad.State               (StateT)
import           Control.Monad.Error               (throwError)
import qualified Data.Binary                       as Bin
import           Data.ByteString                   (ByteString)
import           Data.ByteString.Lazy              (fromStrict)
import qualified Data.IntMap                       as IntMap
import qualified Data.Map                          as Map
import           Data.Maybe                        (fromMaybe, isJust, isNothing)
import qualified Data.Text.Lazy                    as Text
import           Data.List                         (partition, break, stripPrefix)
import           Data.List.Split                   (splitOneOf)
import           Prologue                          hiding (Item)
import           Prologue                          hiding (Item)

import           Empire.API.Data.DefaultValue      (Value (..))
import           Empire.API.Data.GraphLocation     (GraphLocation)
import           Empire.API.Data.Node              (Node (..), NodeId)
import qualified Empire.API.Data.Node              as Node
import qualified Empire.API.Data.NodeSearcher      as NS
import           Empire.API.Data.Port              (InPort (..), OutPort (..), Port (..), PortId (..), PortState (..))
import           Empire.API.Data.PortRef           (InPortRef (..), OutPortRef (..))
import           Empire.API.Data.ValueType         (ValueType (..))
import qualified Empire.API.Graph.AddNode          as AddNode
import qualified Empire.API.Graph.CodeUpdate       as CodeUpdate
import qualified Empire.API.Graph.Connect          as Connect
import qualified Empire.API.Graph.Disconnect       as Disconnect
import qualified Empire.API.Graph.DumpGraphViz     as DumpGraphViz
import qualified Empire.API.Graph.GetProgram       as GetProgram
import qualified Empire.API.Graph.TypeCheck        as TypeCheck
import qualified Empire.API.Graph.NodeResultUpdate as NodeResultUpdate
import qualified Empire.API.Graph.NodeUpdate       as NodeUpdate
import qualified Empire.API.Graph.RemoveNode       as RemoveNode
import qualified Empire.API.Graph.RenameNode       as RenameNode
import qualified Empire.API.Graph.SetDefaultValue  as SetDefaultValue
import qualified Empire.API.Graph.UpdateNodeMeta   as UpdateNodeMeta
import qualified Empire.API.Topic                  as Topic
import qualified Empire.API.Response               as Response
import qualified Empire.Commands.Graph             as Graph
import qualified Empire.Commands.Persistence       as Persistence
import qualified Empire.Empire                     as Empire
import           Empire.Env                        (Env)
import qualified Empire.Env                        as Env
import           Empire.Server.Server              (errorMessage, sendToBus', replyFail, replyResult, replyOk)
import           Flowbox.Bus.BusT                  (BusT (..))
import qualified Flowbox.System.Log.Logger         as Logger
import qualified StdLibMock
import qualified Empire.API.Graph.Request      as G

logger :: Logger.LoggerIO
logger = Logger.getLoggerIO $(Logger.moduleName)

notifyCodeUpdate :: GraphLocation -> StateT Env BusT ()
notifyCodeUpdate location = do
    currentEmpireEnv <- use Env.empireEnv
    empireNotifEnv   <- use Env.empireNotif
    (resultCode, _) <- liftIO $ Empire.runEmpire empireNotifEnv currentEmpireEnv $ Graph.getCode location
    case resultCode of
        Left err -> logger Logger.error $ errorMessage <> err
        Right code -> sendToBus' $ CodeUpdate.Update location $ Text.pack code

notifyNodeResultUpdate :: GraphLocation -> NodeId -> Value -> StateT Env BusT ()
notifyNodeResultUpdate location nodeId value = sendToBus' $ NodeResultUpdate.Update location nodeId (NodeResultUpdate.Value value) 42
-- FIXME: report correct execution time

saveCurrentProject :: GraphLocation -> StateT Env BusT ()
saveCurrentProject loc = do
  currentEmpireEnv <- use Env.empireEnv
  empireNotifEnv   <- use Env.empireNotif
  projectRoot      <- use Env.projectRoot
  void $ liftIO $ Empire.runEmpire empireNotifEnv currentEmpireEnv $ Persistence.saveLocation projectRoot loc

data Expr = Expression        String
          | Function   (Maybe String)
          | Module     (Maybe String)
          | Input      (Maybe String)
          | Output     (Maybe String)

parseExpr :: String -> Expr
parseExpr (stripPrefix "def "  -> Just name)    = Function $ Just name
parseExpr (stripPrefix "module "  -> Just name) = Module   $ Just name
parseExpr (stripPrefix "in "  -> Just name)     = Input    $ Just name
parseExpr (stripPrefix "out "  -> Just name)    = Output   $ Just name
parseExpr "def"                                 = Function   Nothing
parseExpr "module"                              = Module     Nothing
parseExpr "in"                                  = Input      Nothing
parseExpr "out"                                 = Output     Nothing
parseExpr expr                                  = Expression expr

forceTC :: GraphLocation -> StateT Env BusT ()
forceTC location = do
    currentEmpireEnv <- use Env.empireEnv
    empireNotifEnv   <- use Env.empireNotif
    void $ liftIO $ Empire.runEmpire empireNotifEnv currentEmpireEnv $ Graph.typecheck location

modifyGraph :: forall a b c. (G.GraphRequest a, Response.ResponseResult a c) => (a -> Empire.Empire b) -> (a -> b -> StateT Env BusT ()) -> a -> StateT Env BusT ()
modifyGraph action success request = do
    currentEmpireEnv <- use Env.empireEnv
    empireNotifEnv   <- use Env.empireNotif
    (result, newEmpireEnv) <- liftIO $ Empire.runEmpire empireNotifEnv currentEmpireEnv $ action request
    case result of
      Left err     -> replyFail logger err request
      Right result -> do
        Env.empireEnv .= newEmpireEnv
        success request result
        notifyCodeUpdate $ request ^. G.location
        saveCurrentProject $ request ^. G.location

modifyGraphOk :: forall a b c. (Bin.Binary a, G.GraphRequest a, Response.ResponseResult a c, Response.ResponseResult a ()) => (a -> Empire.Empire b) -> (a -> b -> StateT Env BusT ()) -> a -> StateT Env BusT ()
modifyGraphOk action success = modifyGraph action (\req res -> replyOk req >> success req res)

handleAddNode :: AddNode.Request -> StateT Env BusT ()
handleAddNode = modifyGraph action success where
  action (AddNode.Request location nodeType nodeMeta connectTo) = case nodeType of
          AddNode.ExpressionNode expression -> case parseExpr expression of
            Expression expression -> Graph.addNodeCondTC (isNothing connectTo) location (Text.pack $ expression) nodeMeta
            Function name -> throwError "Function Nodes not yet supported"
            Module   name -> throwError "Module Nodes not yet supported"
            Input    name -> throwError "Input Nodes not yet supported"
            Output   name -> throwError "Output Nodes not yet supported"
  success request@(AddNode.Request location nodeType nodeMeta connectTo) node = do
    replyResult request (node ^. Node.nodeId)
    sendToBus' $ AddNode.Update location node
    case nodeType of
        AddNode.ExpressionNode expr -> forM_ connectTo $ \srcNodeId -> do
                let exprCall = head $ splitOneOf " ." expr
                    inPort = if exprCall `elem` stdlibFunctions then Arg 0 else Self
                    connectRequest = Connect.Request location (OutPortRef srcNodeId All) (InPortRef (node ^. Node.nodeId) inPort)
                handleConnectReq False connectRequest
                forceTC location

handleRemoveNode :: RemoveNode.Request -> StateT Env BusT ()
handleRemoveNode = modifyGraphOk action success where
  action (RemoveNode.Request location nodeIds) = Graph.removeNodes location nodeIds
  success request@(RemoveNode.Request location nodeIds) result = do
    sendToBus' $ RemoveNode.Update location nodeIds

handleUpdateNodeMeta :: UpdateNodeMeta.Request -> StateT Env BusT ()
handleUpdateNodeMeta = modifyGraphOk action success where
  action (UpdateNodeMeta.Request location nodeId nodeMeta) = Graph.updateNodeMeta location nodeId nodeMeta
  success request@(UpdateNodeMeta.Request location nodeId nodeMeta) result = do
    sendToBus' $ UpdateNodeMeta.Update location nodeId nodeMeta

handleRenameNode :: RenameNode.Request -> StateT Env BusT ()
handleRenameNode = modifyGraphOk action success where
  action (RenameNode.Request location nodeId name) = Graph.renameNode location nodeId name
  success request@(RenameNode.Request location nodeId name) result = do
    sendToBus' $ RenameNode.Update location nodeId name

handleConnect :: Connect.Request -> StateT Env BusT ()
handleConnect = handleConnectReq True

handleConnectReq :: Bool -> Connect.Request -> StateT Env BusT ()
handleConnectReq doTC = modifyGraphOk action success where
  action (Connect.Request location src dst) = Graph.connectCondTC doTC location src dst
  success request@(Connect.Request location src dst) result = do
    sendToBus' $ Connect.Update location src dst

handleDisconnect :: Disconnect.Request -> StateT Env BusT ()
handleDisconnect = modifyGraphOk action success where
  action request@(Disconnect.Request location dst) = Graph.disconnect location dst
  success request@(Disconnect.Request location dst) result = do
    sendToBus' $ Disconnect.Update location dst

handleSetDefaultValue :: SetDefaultValue.Request -> StateT Env BusT ()
handleSetDefaultValue = modifyGraphOk action success where
  action (SetDefaultValue.Request location portRef defaultValue) = Graph.setDefaultValue location portRef defaultValue
  success _ _ = return ()

stdlibFunctions :: [String]
stdlibFunctions = filter (not . elem '.') StdLibMock.symbolsNames

stdlibMethods :: [String]
stdlibMethods = filter (elem '.') StdLibMock.symbolsNames

handleGetProgram :: GetProgram.Request -> StateT Env BusT ()
handleGetProgram request = do
    let location = request ^. GetProgram.location
    currentEmpireEnv <- use Env.empireEnv
    empireNotifEnv   <- use Env.empireNotif
    (resultGraph, _) <- liftIO $ Empire.runEmpire empireNotifEnv currentEmpireEnv $ Graph.getGraph location
    (resultCode,  _) <- liftIO $ Empire.runEmpire empireNotifEnv currentEmpireEnv $ Graph.getCode  location
    case (resultGraph, resultCode) of
        (Left err, _) -> replyFail logger err request
        (_, Left err) -> replyFail logger err request
        (Right graph, Right code) -> do
            replyResult request $ GetProgram.Result graph (Text.pack code) mockNSData

handleDumpGraphViz :: DumpGraphViz.Request -> StateT Env BusT ()
handleDumpGraphViz request = do
    let location = request ^. DumpGraphViz.location
    currentEmpireEnv <- use Env.empireEnv
    empireNotifEnv   <- use Env.empireNotif
    void $ liftIO $ Empire.runEmpire empireNotifEnv currentEmpireEnv $ Graph.dumpGraphViz location

handleTypecheck :: TypeCheck.Request -> StateT Env BusT ()
handleTypecheck request = do
    let location = request ^. TypeCheck.location
    currentEmpireEnv <- use Env.empireEnv
    empireNotifEnv   <- use Env.empireNotif
    (result, newEmpireEnv) <- liftIO $ Empire.runEmpire empireNotifEnv currentEmpireEnv $ Graph.typecheck location
    case result of
        Left err -> replyFail logger err request
        Right _  -> Env.empireEnv .= newEmpireEnv
    return ()

mockNSData :: NS.Items
mockNSData = Map.fromList $ functionsList <> modulesList where
    nodeSearcherSymbols = filter (not . flip elem StdLibMock.experimental) StdLibMock.symbolsNames
    (methods, functions) = partition (elem '.') nodeSearcherSymbols
    functionsList = functionEntry <$> functions
    functionEntry function = (Text.pack function, NS.Element)
    modulesMethodsMap = foldl updateModulesMethodsMap Map.empty methods
    updateModulesMethodsMap map el = Map.insert moduleName methodNames map where
        (moduleName, dotMethodName) = break (== '.') el
        methodName = tail dotMethodName
        methodNames = methodName : (fromMaybe [] $ Map.lookup moduleName map)
    modulesList = (uncurry moduleEntry) <$> Map.toList modulesMethodsMap
    moduleEntry moduleName methodList = (Text.pack moduleName, NS.Group $ Map.fromList $ functionEntry <$> methodList)

