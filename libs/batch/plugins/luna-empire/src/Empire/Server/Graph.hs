{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns      #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE RankNTypes  #-}

module Empire.Server.Graph where

import           Control.Monad.State                   (StateT)
import           Control.Monad.Error                   (throwError)
import qualified Data.Binary                           as Bin
import           Data.UUID.Types                       (UUID)
import qualified Data.UUID.Types                       as UUID
import qualified Data.UUID.V4                          as UUID
import           Data.ByteString                       (ByteString)
import           Data.ByteString.Lazy                  (fromStrict)
import qualified Data.IntMap                           as IntMap
import qualified Data.Map                              as Map
import           Data.Maybe                            (fromMaybe, isJust, isNothing)
import qualified Data.Text.Lazy                        as Text
import           Data.Text.Lazy                        (Text, stripPrefix)
import           Data.List                             (partition, break)
import           Data.List.Split                       (splitOneOf)
import           Prologue                              hiding (Item)

import           Empire.API.Data.DefaultValue          (Value (..))
import           Empire.API.Data.GraphLocation         (GraphLocation)
import           Empire.API.Data.Node                  (Node (..), NodeId)
import qualified Empire.API.Data.Node                  as Node
import qualified Empire.API.Data.NodeSearcher          as NS
import           Empire.API.Data.Port                  (InPort (..), OutPort (..), Port (..), PortId (..), PortState (..))
import           Empire.API.Data.PortRef               (InPortRef (..), OutPortRef (..))
import           Empire.API.Data.ValueType             (ValueType (..))
import qualified Empire.API.Graph.AddNode              as AddNode
import qualified Empire.API.Graph.CodeUpdate           as CodeUpdate
import qualified Empire.API.Graph.Connect              as Connect
import qualified Empire.API.Graph.Disconnect           as Disconnect
import qualified Empire.API.Graph.DumpGraphViz         as DumpGraphViz
import qualified Empire.API.Graph.GetProgram           as GetProgram
import qualified Empire.API.Graph.TypeCheck            as TypeCheck
import qualified Empire.API.Graph.NodeResultUpdate     as NodeResultUpdate
import qualified Empire.API.Graph.NodeUpdate           as NodeUpdate
import qualified Empire.API.Graph.RemoveNode           as RemoveNode
import qualified Empire.API.Graph.RenameNode           as RenameNode
import qualified Empire.API.Graph.SetDefaultValue      as SetDefaultValue
import qualified Empire.API.Graph.UpdateNodeExpression as UpdateNodeExpression
import qualified Empire.API.Graph.UpdateNodeMeta       as UpdateNodeMeta
import qualified Empire.API.Topic                      as Topic
import qualified Empire.API.Response                   as Response
import           Empire.API.Request                    (Request(..))
import qualified Empire.Commands.Graph                 as Graph
import qualified Empire.Commands.Persistence           as Persistence
import qualified Empire.Empire                         as Empire
import           Empire.Env                            (Env)
import qualified Empire.Env                            as Env
import           Empire.Server.Server                  (errorMessage, sendToBus', replyFail, replyResult, replyOk)
import           Flowbox.Bus.BusT                      (BusT (..))
import qualified Flowbox.System.Log.Logger             as Logger
import qualified StdLibMock
import qualified Empire.API.Graph.Request              as G
import           Empire.Utils.TextResult               (nodeValueToText)


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

notifyNodeResultUpdate :: GraphLocation -> NodeId -> [Value] -> Text -> StateT Env BusT ()
notifyNodeResultUpdate location nodeId values name = sendToBus' $ NodeResultUpdate.Update location nodeId (NodeResultUpdate.Value name values) 42
-- FIXME: report correct execution time

saveCurrentProject :: GraphLocation -> StateT Env BusT ()
saveCurrentProject loc = do
  currentEmpireEnv <- use Env.empireEnv
  empireNotifEnv   <- use Env.empireNotif
  projectRoot      <- use Env.projectRoot
  void $ liftIO $ Empire.runEmpire empireNotifEnv currentEmpireEnv $ Persistence.saveLocation projectRoot loc

data Expr = Expression        Text
          | Function   (Maybe Text)
          | Module     (Maybe Text)
          | Input      (Maybe Text)
          | Output     (Maybe Text)

parseExpr :: Text -> Expr
parseExpr (stripPrefix "def "    -> Just name) = Function $ Just name
parseExpr (stripPrefix "module " -> Just name) = Module   $ Just name
parseExpr (stripPrefix "in "     -> Just name) = Input    $ Just name
parseExpr (stripPrefix "out "    -> Just name) = Output   $ Just name
parseExpr "def"                                = Function   Nothing
parseExpr "module"                             = Module     Nothing
parseExpr "in"                                 = Input      Nothing
parseExpr "out"                                = Output     Nothing
parseExpr expr                                 = Expression expr

forceTC :: GraphLocation -> StateT Env BusT ()
forceTC location = do
    currentEmpireEnv <- use Env.empireEnv
    empireNotifEnv   <- use Env.empireNotif
    void $ liftIO $ Empire.runEmpire empireNotifEnv currentEmpireEnv $ Graph.typecheck location

modifyGraph :: forall a b c. (G.GraphRequest a, Response.ResponseResult a c) => (a -> Empire.Empire b) -> (Request a -> b -> StateT Env BusT ()) -> Request a -> StateT Env BusT ()
modifyGraph action success req@(Request uuid request) = do
    currentEmpireEnv <- use Env.empireEnv
    empireNotifEnv   <- use Env.empireNotif
    (result, newEmpireEnv) <- liftIO $ Empire.runEmpire empireNotifEnv currentEmpireEnv $ action request
    case result of
      Left err     -> replyFail logger err req
      Right result -> do
        Env.empireEnv .= newEmpireEnv
        success req result
        notifyCodeUpdate $ request ^. G.location
        saveCurrentProject $ request ^. G.location

modifyGraphOk :: forall a b c. (Bin.Binary a, G.GraphRequest a, Response.ResponseResult a c, Response.ResponseResult a ()) => (a -> Empire.Empire b) -> (a -> b -> StateT Env BusT ()) -> Request a -> StateT Env BusT ()
modifyGraphOk action success = modifyGraph action (\req@(Request uuid request) res -> replyOk req >> success request res)

handleAddNode :: Request AddNode.Request -> StateT Env BusT ()
handleAddNode = modifyGraph action success where
  action (AddNode.Request location nodeType nodeMeta connectTo) = case nodeType of
          AddNode.ExpressionNode expression -> case parseExpr expression of
            Expression expression -> do
              uuid <- case expression of
                  "inside"               -> return $ fromJust $ UUID.fromString "1a9eeda0-4627-4597-9d09-167c44cbe87e"
                  "outside"              -> return $ fromJust $ UUID.fromString "1a9eeda0-4627-4597-9d09-167c44cbe87f"
                  "temperatureThreshold" -> return $ fromJust $ UUID.fromString "77962bb1-32f1-4f4d-ae5f-4328b7c83708"
                  _  -> liftIO $ UUID.nextRandom

              Graph.addNodeCondTC (isNothing connectTo) location uuid expression nodeMeta
            Function name -> throwError "Function Nodes not yet supported"
            Module   name -> throwError "Module Nodes not yet supported"
            Input    name -> throwError "Input Nodes not yet supported"
            Output   name -> throwError "Output Nodes not yet supported"
  success request@(Request _ req@(AddNode.Request location nodeType nodeMeta connectTo)) node = do
    replyResult request (node ^. Node.nodeId)
    sendToBus' $ AddNode.Update location node
    case nodeType of
        AddNode.ExpressionNode expr -> forM_ connectTo $ \srcNodeId -> do
                let exprCall = head $ splitOneOf " ." $ Text.unpack expr
                    inPort = if exprCall `elem` stdlibFunctions then Arg 0 else Self
                    connectRequest = Request UUID.nil $ Connect.Request location (OutPortRef srcNodeId All) (InPortRef (node ^. Node.nodeId) inPort)
                handleConnectReq False connectRequest
                forceTC location

handleRemoveNode :: Request RemoveNode.Request -> StateT Env BusT ()
handleRemoveNode = modifyGraphOk action success where
  action  (RemoveNode.Request location nodeIds) = Graph.removeNodes location nodeIds
  success (RemoveNode.Request location nodeIds) result = sendToBus' $ RemoveNode.Update location nodeIds

handleUpdateNodeExpression :: Request UpdateNodeExpression.Request -> StateT Env BusT ()
handleUpdateNodeExpression = modifyGraphOk action success where
  action (UpdateNodeExpression.Request location nodeId expression) = Graph.updateNodeExpression location nodeId expression
  success _ _ = return ()

handleUpdateNodeMeta :: Request UpdateNodeMeta.Request -> StateT Env BusT ()
handleUpdateNodeMeta = modifyGraphOk action success where
  action (UpdateNodeMeta.Request location nodeId nodeMeta) = Graph.updateNodeMeta location nodeId nodeMeta
  success (UpdateNodeMeta.Request location nodeId nodeMeta) result = sendToBus' $ UpdateNodeMeta.Update location nodeId nodeMeta

handleRenameNode :: Request RenameNode.Request -> StateT Env BusT ()
handleRenameNode = modifyGraphOk action success where
  action (RenameNode.Request location nodeId name) = Graph.renameNode location nodeId name
  success (RenameNode.Request location nodeId name) result = sendToBus' $ RenameNode.Update location nodeId name

handleConnect :: Request Connect.Request -> StateT Env BusT ()
handleConnect = handleConnectReq True

handleConnectReq :: Bool -> Request Connect.Request -> StateT Env BusT ()
handleConnectReq doTC = modifyGraphOk action success where
  action (Connect.Request location src dst) = Graph.connectCondTC doTC location src dst
  success (Connect.Request location src dst) result = sendToBus' $ Connect.Update location src dst

handleDisconnect :: Request Disconnect.Request -> StateT Env BusT ()
handleDisconnect = modifyGraphOk action success where
  action (Disconnect.Request location dst) = Graph.disconnect location dst
  success (Disconnect.Request location dst) result = sendToBus' $ Disconnect.Update location dst

handleSetDefaultValue :: Request SetDefaultValue.Request -> StateT Env BusT ()
handleSetDefaultValue = modifyGraphOk action success where
  action (SetDefaultValue.Request location portRef defaultValue) = Graph.setDefaultValue location portRef defaultValue
  success _ _ = return ()

stdlibFunctions :: [String]
stdlibFunctions = filter (not . elem '.') StdLibMock.symbolsNames

stdlibMethods :: [String]
stdlibMethods = filter (elem '.') StdLibMock.symbolsNames

handleGetProgram :: Request GetProgram.Request -> StateT Env BusT ()
handleGetProgram req@(Request uuid request) = do
    let location = request ^. GetProgram.location
    currentEmpireEnv <- use Env.empireEnv
    empireNotifEnv   <- use Env.empireNotif
    (resultGraph, _) <- liftIO $ Empire.runEmpire empireNotifEnv currentEmpireEnv $ Graph.getGraph location
    (resultCode,  _) <- liftIO $ Empire.runEmpire empireNotifEnv currentEmpireEnv $ Graph.getCode  location
    case (resultGraph, resultCode) of
        (Left err, _) -> replyFail logger err req
        (_, Left err) -> replyFail logger err req
        (Right graph, Right code) -> do
            replyResult req $ GetProgram.Result graph (Text.pack code) mockNSData

handleDumpGraphViz :: Request DumpGraphViz.Request -> StateT Env BusT ()
handleDumpGraphViz (Request _ request) = do
    let location = request ^. DumpGraphViz.location
    currentEmpireEnv <- use Env.empireEnv
    empireNotifEnv   <- use Env.empireNotif
    void $ liftIO $ Empire.runEmpire empireNotifEnv currentEmpireEnv $ Graph.dumpGraphViz location

handleTypecheck :: Request TypeCheck.Request -> StateT Env BusT ()
handleTypecheck req@(Request _ request) = do
    let location = request ^. TypeCheck.location
    currentEmpireEnv <- use Env.empireEnv
    empireNotifEnv   <- use Env.empireNotif
    (result, newEmpireEnv) <- liftIO $ Empire.runEmpire empireNotifEnv currentEmpireEnv $ Graph.typecheck location
    case result of
        Left err -> replyFail logger err req
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

