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
import           Data.ByteString.Lazy      hiding (head)

type Action = (IO (), Maybe Breadcrumbs)

react :: Project -> Ev.Event Dynamic -> Maybe Action
react project (Ev.Backend (Backend.Message msg)) = Just $ reactToMessage project msg
react _       _                                  = Nothing

reactToMessage :: Project -> WebMessage -> Action
reactToMessage project (WebMessage topic bytes) = case topic of
    "project.library.ast.function.add.update"       -> handleFunctionCreation project bytes
    "project.library.ast.function.graph.get.status" -> printGraph bytes
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
    sendMessage $ getGraph crumbs project lib

printGraph :: ByteString -> Action
printGraph bytes = case parseGraphViewResponse bytes of
    Nothing -> (print "Error parsing graph", Nothing)
    Just g  -> (print g, Nothing)

makeNetworkDescription :: forall t. Frameworks t => Project -> WebSocket -> IO () -> Moment t ()
makeNetworkDescription project socket callback = do
    webSocketE <- fromAddHandler $ webSocketHandler socket
    let actions = filterJust $ react project <$> webSocketE
        crumbs  = filterJust $ snd <$> actions
    reactimate $ fst <$> actions
    reactimate $ callback <$ crumbs

run :: WebSocket -> IO () -> Project -> IO ()
run socket callback project = do
    print "Setting project id"
    sendMessage $ setProjectId project
    print "Creating function"
    sendMessage $ createMainFunction project (head $ project ^. libs)
    net <- compile $ makeNetworkDescription project socket callback
    actuate net
