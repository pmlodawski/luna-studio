module Reactive.Plugins.Loader.Interpreter.Network where

import Utils.PreludePlus
import Batch.Project
import Batch.Breadcrumbs
import BatchConnector.Commands
import BatchConnector.Connection

import Reactive.Banana
import Reactive.Banana.Frameworks
import Reactive.Handlers

import           Data.Dynamic
import           JS.WebSocket
import qualified Event.Event               as Ev
import qualified Event.Backend             as Backend
import           BatchConnector.Connection
import           BatchConnector.Commands
import           BatchConnector.Updates
import           Data.ByteString.Lazy      (ByteString)
import           Batch.Function            (moduleBreadcrumbs)
import           Batch.Workspace

type Action = (IO (), Maybe Breadcrumbs)

react :: Project -> Ev.Event Dynamic -> Maybe Action
react project (Ev.Backend (Backend.Message msg)) = Just $ reactToMessage project msg
react _       _                                  = Nothing

reactToMessage :: Project -> WebMessage -> Action
reactToMessage project (WebMessage topic bytes) = case topic of
    "project.library.ast.function.add.update"       -> handleFunctionCreation project bytes
    "project.library.ast.function.graph.get.status" -> printGraph bytes
    "project.library.ast.code.get.status"           -> printCode bytes
    "interpreter.value.update"                      -> printValue bytes
    _                                               -> (return (), Nothing)

handleFunctionCreation :: Project -> ByteString -> Action
handleFunctionCreation project bytes = case parseFunctionCreateResponse bytes of
    Nothing     -> (print "Something went terribly wrong!", Nothing)
    Just crumbs -> (setMain project crumbs, Just crumbs)

setMain :: Project -> Breadcrumbs -> IO ()
setMain project crumbs = do
    print "Crumbs ready!"
    print crumbs
    let lib = head $ project ^. libs
    sendMessage $ setMainPtr project (head $ project ^. libs) crumbs
    sendMessage runMain
    sendMessage $ getGraph project lib crumbs
    sendMessage $ getCode project lib crumbs

printCode :: ByteString -> Action
printCode bytes = case parseGetCodeResponse bytes of
    Nothing   -> (putStrLn "Something went wrong!", Nothing)
    Just code -> (putStrLn "This is code:" >> putStrLn code, Nothing)

printGraph :: ByteString -> Action
printGraph bytes = case parseGraphViewResponse bytes of
    Nothing -> (print "Error parsing graph", Nothing)
    Just g  -> (print g, Nothing)

printValue :: ByteString -> Action
printValue bytes = case parseValueUpdate bytes of
    Nothing -> (print "Error parsing value!", Nothing)
    Just v  -> (print v, Nothing)

makeNetworkDescription :: forall t. Frameworks t => Project -> WebSocket -> (Workspace -> IO ()) -> Moment t ()
makeNetworkDescription project socket callback = do
    webSocketE <- fromAddHandler $ webSocketHandler socket

    let actions   = filterJust $ react project <$> webSocketE
        crumbs    = filterJust $ snd <$> actions
        workspace = Workspace project (head $ project ^. libs) <$> crumbs

    reactimate $ fst <$> actions
    reactimate $ callback <$> workspace

run :: WebSocket -> (Workspace -> IO ()) -> Project -> IO ()
run socket callback project = do
    print "Setting project id"
    sendMessage $ setProjectId project
    print "Creating function"
    sendMessage $ createMainFunction project (head $ project ^. libs)
    net <- compile $ makeNetworkDescription project socket callback
    actuate net
