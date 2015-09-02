module Reactive.Plugins.Loader.InterpreterTest where

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
import qualified Event.Event as Ev
import qualified Event.Backend as Backend
import           BatchConnector.Connection
import           BatchConnector.Commands
import           BatchConnector.Updates
import           Data.ByteString.Lazy     hiding (head)

react :: Project -> Ev.Event Dynamic -> Maybe (IO ())
react proj (Ev.Backend (Backend.Message msg)) = Just $ reactToMessage proj msg
react _    _                                  = Nothing

reactToMessage :: Project -> WebMessage -> IO ()
reactToMessage proj (WebMessage topic bytes) = case topic of
    "project.library.ast.function.add.update" -> handleFunctionCreation proj bytes
    _                                         -> print $ "Unexpected message: " ++ topic

handleFunctionCreation :: Project -> ByteString -> IO ()
handleFunctionCreation proj bytes = case parseFunctionCreateResponse bytes of
    Nothing     -> print "Something went terribly wrong!"
    Just crumbs -> experimentWithCrumbs proj crumbs

experimentWithCrumbs :: Project -> Breadcrumbs -> IO ()
experimentWithCrumbs proj crumbs = do
    print crumbs
    sendMessage $ setMainPtr proj (head $ proj ^. libs) crumbs
    sendMessage runMain

makeNetworkDescription :: forall t. Frameworks t => Project -> WebSocket -> Moment t ()
makeNetworkDescription project socket = do
    webSocketE <- fromAddHandler $ webSocketHandler socket
    let actions = filterJust $ react project <$> webSocketE
    reactimate $ actions

testInterpreter :: WebSocket -> Project -> IO ()
testInterpreter socket project = do
    print "Setting project id"
    sendMessage $ setProjectId project
    print "Creating function"
    sendMessage $ createMainFunction project (head $ project ^. libs)
    net <- compile $ makeNetworkDescription project socket
    actuate net
