{-# LANGUAGE OverloadedStrings #-}

module Empire.Server.Graph where

import           Control.Monad.State               (StateT)
import qualified Data.Binary                       as Bin
import           Data.ByteString                   (ByteString)
import           Data.ByteString.Lazy              (fromStrict)
import qualified Data.IntMap                       as IntMap
import qualified Data.Map                          as Map
import           Data.Maybe                        (fromMaybe, isJust, isNothing)
import qualified Data.Text.Lazy                    as Text
import           Data.List.Split                   (splitOneOf)
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
import qualified Empire.API.Update                 as Update
import qualified Empire.Commands.Graph             as Graph
import qualified Empire.Empire                     as Empire
import           Empire.Env                        (Env)
import qualified Empire.Env                        as Env
import           Empire.Server.Server              (errorMessage, sendToBus)
import           Flowbox.Bus.BusT                  (BusT (..))
import qualified Flowbox.System.Log.Logger         as Logger
import qualified StdLibMock

logger :: Logger.LoggerIO
logger = Logger.getLoggerIO $(Logger.moduleName)

notifyCodeUpdate :: GraphLocation -> StateT Env BusT ()
notifyCodeUpdate location = do
    currentEmpireEnv <- use Env.empireEnv
    empireNotifEnv   <- use Env.empireNotif
    (resultCode, _) <- liftIO $ Empire.runEmpire empireNotifEnv currentEmpireEnv $ Graph.getCode location
    case resultCode of
        Left err -> logger Logger.error $ errorMessage <> err
        Right code -> do
            let update = CodeUpdate.Update location $ Text.pack code
            sendToBus Topic.codeUpdate update

notifyNodeResultUpdate :: GraphLocation -> NodeId -> Value -> StateT Env BusT ()
notifyNodeResultUpdate location nodeId value = do
    let update = NodeResultUpdate.Update location nodeId (NodeResultUpdate.Value value) 42 -- FIXME: report correct execution time
    sendToBus Topic.nodeResultUpdate update


data Expr = Expression String | Function (Maybe String) | Module (Maybe String) | Input (Maybe String)| Output (Maybe String)

parseExpr :: String -> Expr
parseExpr ('d':'e':'f':' ':name) = Function $ Just name
parseExpr "def"          = Function Nothing
parseExpr ('m':'o':'d':'u':'l':'e':' ':name) = Module $ Just name
parseExpr "module" = Module Nothing
parseExpr ('i':'n':' ':name) = Input $ Just name
parseExpr "in" = Input Nothing
parseExpr ('o':'u':'t':' ':name) = Output $ Just name
parseExpr "out" = Output Nothing
parseExpr expr = Expression expr

stdlibFunctions :: [String]
stdlibFunctions = filter (not . elem '.') StdLibMock.symbolsNames

forceTC :: GraphLocation -> StateT Env BusT ()
forceTC location = do
    currentEmpireEnv <- use Env.empireEnv
    empireNotifEnv   <- use Env.empireNotif
    void $ liftIO $ Empire.runEmpire empireNotifEnv currentEmpireEnv $ Graph.typecheck location

handleAddNode :: ByteString -> StateT Env BusT ()
handleAddNode content = do
    let request   = Bin.decode . fromStrict $ content :: AddNode.Request
        location  = request ^. AddNode.location
        connectTo = request ^. AddNode.connectTo
    currentEmpireEnv <- use Env.empireEnv
    empireNotifEnv   <- use Env.empireNotif
    (result, newEmpireEnv) <- case request ^. AddNode.nodeType of
      AddNode.ExpressionNode expression -> case parseExpr expression of
        Expression expression -> liftIO $ Empire.runEmpire empireNotifEnv currentEmpireEnv $ Graph.addNodeCondTC
            (isNothing connectTo)
            location
            (Text.pack $ expression)
            (request ^. AddNode.nodeMeta)
        Function name -> return (Left "Function Nodes not yet supported", currentEmpireEnv)
        Module   name -> return (Left "Module Nodes not yet supported",   currentEmpireEnv)
        Input    name -> return (Left "Input Nodes not yet supported",    currentEmpireEnv)
        Output   name -> return (Left "Output Nodes not yet supported",   currentEmpireEnv)
      AddNode.InputNode _ _ -> return (Left "Input Nodes not yet supported", currentEmpireEnv)
    case result of
        Left err -> logger Logger.error $ errorMessage <> err
        Right node -> do
            Env.empireEnv .= newEmpireEnv
            let update = Update.Update request $ AddNode.Result node
            sendToBus Topic.addNodeUpdate update
            case request ^. AddNode.nodeType of
                AddNode.InputNode _ _ -> return ()
                AddNode.ExpressionNode expr -> forM_ connectTo $ \srcNodeId -> do
                        let exprCall = head $ splitOneOf " ." expr
                            inPort = if exprCall `elem` stdlibFunctions then Arg 0 else Self
                            connectRequest = Connect.Request location srcNodeId All (node ^. Node.nodeId) inPort
                        handleConnectReq False connectRequest
                        forceTC location
            notifyCodeUpdate location

handleRemoveNode :: ByteString -> StateT Env BusT ()
handleRemoveNode content = do
    let request = Bin.decode . fromStrict $ content :: RemoveNode.Request
        location = request ^. RemoveNode.location
    currentEmpireEnv <- use Env.empireEnv
    empireNotifEnv   <- use Env.empireNotif
    (result, newEmpireEnv) <- liftIO $ Empire.runEmpire empireNotifEnv currentEmpireEnv $ Graph.removeNodes
        location
        (request ^. RemoveNode.nodeIds)
    case result of
        Left err -> logger Logger.error $ errorMessage <> err
        Right _ -> do
            Env.empireEnv .= newEmpireEnv
            let update = Update.Update request $ Update.Ok
            sendToBus Topic.removeNodeUpdate update
            notifyCodeUpdate location

handleUpdateNodeMeta :: ByteString -> StateT Env BusT ()
handleUpdateNodeMeta content = do
    let request = Bin.decode . fromStrict $ content :: UpdateNodeMeta.Request
        nodeMeta = request ^. UpdateNodeMeta.nodeMeta
    currentEmpireEnv <- use Env.empireEnv
    empireNotifEnv   <- use Env.empireNotif
    (result, newEmpireEnv) <- liftIO $ Empire.runEmpire empireNotifEnv currentEmpireEnv $ Graph.updateNodeMeta
        (request ^. UpdateNodeMeta.location)
        (request ^. UpdateNodeMeta.nodeId)
        nodeMeta
    case result of
        Left err -> logger Logger.error $ errorMessage <> err
        Right _ -> do
            Env.empireEnv .= newEmpireEnv
            let update = Update.Update request $ UpdateNodeMeta.Result nodeMeta
            sendToBus Topic.updateNodeMetaUpdate update
            notifyCodeUpdate $ request ^. UpdateNodeMeta.location

handleRenameNode :: ByteString -> StateT Env BusT ()
handleRenameNode content = do
    let request  = Bin.decode . fromStrict $ content :: RenameNode.Request
        location = request ^. RenameNode.location
    currentEmpireEnv <- use Env.empireEnv
    empireNotifEnv   <- use Env.empireNotif
    (result, newEmpireEnv) <- liftIO $ Empire.runEmpire empireNotifEnv currentEmpireEnv $ Graph.renameNode
        (request ^. RenameNode.location)
        (request ^. RenameNode.nodeId)
        (request ^. RenameNode.name)
    case result of
        Left err -> logger Logger.error $ errorMessage <> err
        Right _ -> do
            Env.empireEnv .= newEmpireEnv
            let update = Update.Update request $ Update.Ok
            sendToBus Topic.renameNodeUpdate update
            notifyCodeUpdate location

handleConnect :: ByteString -> StateT Env BusT ()
handleConnect content = handleConnectReq True $ Bin.decode . fromStrict $ content

handleConnectReq :: Bool -> Connect.Request -> StateT Env BusT ()
handleConnectReq doTC request = do
    let location = request ^. Connect.location
    currentEmpireEnv <- use Env.empireEnv
    empireNotifEnv   <- use Env.empireNotif
    (result, newEmpireEnv) <- liftIO $ Empire.runEmpire empireNotifEnv currentEmpireEnv $ Graph.connectCondTC
        doTC
        location
        (request ^. Connect.src)
        (request ^. Connect.dst)
    case result of
        Left err -> logger Logger.error $ errorMessage <> err
        Right node -> do
            Env.empireEnv .= newEmpireEnv
            let update = Update.Update request $ Update.Ok
            sendToBus Topic.connectUpdate update
            notifyCodeUpdate location

handleDisconnect :: ByteString -> StateT Env BusT ()
handleDisconnect content = do
    let request = Bin.decode . fromStrict $ content :: Disconnect.Request
        location = request ^. Disconnect.location
    currentEmpireEnv <- use Env.empireEnv
    empireNotifEnv   <- use Env.empireNotif
    (result, newEmpireEnv) <- liftIO $ Empire.runEmpire empireNotifEnv currentEmpireEnv $ Graph.disconnect
        location
        (request ^. Disconnect.dst)
    case result of
        Left err -> logger Logger.error $ errorMessage <> err
        Right node -> do
            Env.empireEnv .= newEmpireEnv
            let update = Update.Update request $ Update.Ok
            sendToBus Topic.disconnectUpdate update
            notifyCodeUpdate location

handleSetDefaultValue :: ByteString -> StateT Env BusT ()
handleSetDefaultValue content = do
    let request = Bin.decode . fromStrict $ content :: SetDefaultValue.Request
        location = request ^. SetDefaultValue.location
    currentEmpireEnv <- use Env.empireEnv
    empireNotifEnv   <- use Env.empireNotif
    (result, newEmpireEnv) <- liftIO $ Empire.runEmpire empireNotifEnv currentEmpireEnv $ Graph.setDefaultValue
        location
        (request ^. SetDefaultValue.portRef)
        (request ^. SetDefaultValue.defaultValue)
    case result of
        Left err -> logger Logger.error $ errorMessage <> err
        Right node -> do
            Env.empireEnv .= newEmpireEnv
            let update = Update.Update request $ Update.Ok
            sendToBus Topic.setDefaultValueUpdate update
            notifyCodeUpdate location

mockNSData  = NS.LunaModule $ Map.fromList  [
      ("id"             , NS.Function)
    , ("const"          , NS.Function)
    , ("app"            , NS.Function)
    , ("comp"           , NS.Function)
    , ("flip"           , NS.Function)
    , ("empty"          , NS.Function)
    , ("singleton"      , NS.Function)
    , ("switch"         , NS.Function)
    , ("readFile"       , NS.Function)
    , ("mean"           , NS.Function)
    , ("differences"    , NS.Function)
    , ("histogram"      , NS.Function)
    , ("primes"         , NS.Function)
    , ("List"           , NS.Module $ NS.LunaModule $ Map.fromList [
                                          ("+"          , NS.Function)
                                        , ("append"     , NS.Function)
                                        , ("prepend"    , NS.Function)
                                        , ("length"     , NS.Function)
                                        , ("reverse"    , NS.Function)
                                        , ("take"       , NS.Function)
                                        , ("drop"       , NS.Function)
                                        , ("sort"       , NS.Function)

                                        , ("fold"       , NS.Function)
                                        , ("map"        , NS.Function)
                                        , ("zip"        , NS.Function)
                                        , ("filter"     , NS.Function)
                                        ])
    , ("Int"            , NS.Module $ NS.LunaModule $ Map.fromList [
                                          ("=="         , NS.Function)
                                        , ("/="         , NS.Function)
                                        , ("<"          , NS.Function)
                                        , ("<="         , NS.Function)
                                        , (">"          , NS.Function)
                                        , (">="         , NS.Function)
                                        , ("min"        , NS.Function)
                                        , ("max"        , NS.Function)

                                        , ("+"          , NS.Function)
                                        , ("*"          , NS.Function)
                                        , ("-"          , NS.Function)
                                        , ("/"          , NS.Function)
                                        , ("%"          , NS.Function)
                                        , ("^"          , NS.Function)

                                        , ("negate"     , NS.Function)
                                        , ("abs"        , NS.Function)
                                        , ("signum"     , NS.Function)

                                        , ("pred"       , NS.Function)
                                        , ("succ"       , NS.Function)
                                        , ("even"       , NS.Function)
                                        , ("odd"        , NS.Function)

                                        , ("gcd"        , NS.Function)
                                        , ("lcm"        , NS.Function)

                                        , ("times"      , NS.Function)
                                        , ("upto"       , NS.Function)

                                        , ("toDouble"   , NS.Function)
                                        , ("toString"   , NS.Function)
                                        ])
    , ("Double"         , NS.Module $ NS.LunaModule $ Map.fromList [
                                          ("=="         , NS.Function)
                                        , ("/="         , NS.Function)
                                        , ("<"          , NS.Function)
                                        , ("<="         , NS.Function)
                                        , (">"          , NS.Function)
                                        , (">="         , NS.Function)
                                        , ("min"        , NS.Function)
                                        , ("max"        , NS.Function)

                                        , ("+"          , NS.Function)
                                        , ("*"          , NS.Function)
                                        , ("-"          , NS.Function)
                                        , ("/"          , NS.Function)
                                        , ("**"         , NS.Function)

                                        , ("negate"     , NS.Function)
                                        , ("abs"        , NS.Function)
                                        , ("signum"     , NS.Function)

                                        , ("round"      , NS.Function)
                                        , ("ceiling"    , NS.Function)
                                        , ("floor"      , NS.Function)

                                        , ("exp"        , NS.Function)
                                        , ("log"        , NS.Function)
                                        , ("sqrt"       , NS.Function)

                                        , ("sin"        , NS.Function)
                                        , ("cos"        , NS.Function)
                                        , ("tan"        , NS.Function)
                                        , ("asin"       , NS.Function)
                                        , ("acos"       , NS.Function)
                                        , ("atan"       , NS.Function)
                                        , ("sinh"       , NS.Function)
                                        , ("cosh"       , NS.Function)
                                        , ("tanh"       , NS.Function)
                                        , ("asinh"      , NS.Function)
                                        , ("acosh"      , NS.Function)
                                        , ("atanh"      , NS.Function)

                                        , ("toString"   , NS.Function)
                                        ])
    , ("Bool",            NS.Module $ NS.LunaModule $ Map.fromList [
                                          ("=="         , NS.Function)
                                        , ("/="         , NS.Function)
                                        , ("<"          , NS.Function)
                                        , ("<="         , NS.Function)
                                        , (">"          , NS.Function)
                                        , (">="         , NS.Function)
                                        , ("min"        , NS.Function)
                                        , ("max"        , NS.Function)

                                        , ("&&"         , NS.Function)
                                        , ("||"         , NS.Function)
                                        , ("not"        , NS.Function)

                                        , ("toString"   , NS.Function)
                                        ])
    , ("String"         , NS.Module $ NS.LunaModule $ Map.fromList [
                                          ("=="         , NS.Function)
                                        , ("/="         , NS.Function)
                                        , ("<"          , NS.Function)
                                        , ("<="         , NS.Function)
                                        , (">"          , NS.Function)
                                        , (">="         , NS.Function)
                                        , ("min"        , NS.Function)
                                        , ("max"        , NS.Function)

                                        , ("+"          , NS.Function)
                                        , ("length"     , NS.Function)
                                        , ("reverse"    , NS.Function)
                                        , ("words"      , NS.Function)
                                        , ("lines"      , NS.Function)
                                        , ("join"       , NS.Function)

                                        , ("toString"   , NS.Function)
                                        ])
    ]

handleGetProgram :: ByteString -> StateT Env BusT ()
handleGetProgram content = do
    let request = Bin.decode . fromStrict $ content :: GetProgram.Request
        location = request ^. GetProgram.location
    currentEmpireEnv <- use Env.empireEnv
    empireNotifEnv   <- use Env.empireNotif
    (resultGraph, _) <- liftIO $ Empire.runEmpire empireNotifEnv currentEmpireEnv $ Graph.getGraph location
    (resultCode,  _) <- liftIO $ Empire.runEmpire empireNotifEnv currentEmpireEnv $ Graph.getCode  location
    case (resultGraph, resultCode) of
        (Left err, _) -> logger Logger.error $ errorMessage <> err
        (_, Left err) -> logger Logger.error $ errorMessage <> err
        (Right graph, Right code) -> do
            let update = Update.Update request $ GetProgram.Status graph (Text.pack code) mockNSData
            sendToBus Topic.programStatus update

handleDumpGraphViz :: ByteString -> StateT Env BusT ()
handleDumpGraphViz content = do
    let request = Bin.decode . fromStrict $ content :: DumpGraphViz.Request
        location = request ^. DumpGraphViz.location
    currentEmpireEnv <- use Env.empireEnv
    empireNotifEnv   <- use Env.empireNotif
    void $ liftIO $ Empire.runEmpire empireNotifEnv currentEmpireEnv $ Graph.dumpGraphViz location

handleTypecheck :: ByteString -> StateT Env BusT ()
handleTypecheck content = do
    let request  = Bin.decode . fromStrict $ content :: TypeCheck.Request
        location = request ^. TypeCheck.location
    currentEmpireEnv <- use Env.empireEnv
    empireNotifEnv   <- use Env.empireNotif
    (result, newEmpireEnv) <- liftIO $ Empire.runEmpire empireNotifEnv currentEmpireEnv $ Graph.typecheck location
    case result of
        Left err -> logger Logger.error $ errorMessage <> err
        Right _  -> Env.empireEnv .= newEmpireEnv
    return ()
