module Reactive.Plugins.Core.Network where

import           Utils.PreludePlus

import           Control.Concurrent.MVar
import           Control.Exception                                   (catch)
import           Data.Monoid                                         (Last (..))
import           GHCJS.Prim                                          (JSException)


import           Reactive.Handlers                                   (AddHandler (..))
import qualified Reactive.Handlers                                   as Handlers

import qualified Event.Event                                         as Event
import qualified Event.Processors.Batch                              as BatchEventProcessor
import qualified Event.Processors.CustomEvent                        as CustomEventProcessor

import qualified Reactive.Plugins.Core.Action.Backend.Control        as Control
import qualified Reactive.Plugins.Core.Action.Backend.Graph          as Graph
import qualified Reactive.Plugins.Core.Action.Backend.ProjectManager as ProjectManager
import qualified Reactive.Plugins.Core.Action.Camera                 as Camera
import qualified Reactive.Plugins.Core.Action.Connect                as Connect
import qualified Reactive.Plugins.Core.Action.ConnectionPen          as ConnectionPen
import qualified Reactive.Plugins.Core.Action.Debug                  as Debug
import qualified Reactive.Plugins.Core.Action.Drag                   as Drag
import qualified Reactive.Plugins.Core.Action.General                as General
import qualified Reactive.Plugins.Core.Action.MultiSelection         as MultiSelection
import qualified Reactive.Plugins.Core.Action.NodeSearcher           as NodeSearcher
import qualified Reactive.Plugins.Core.Action.Sandbox                as Sandbox
import qualified Reactive.Plugins.Core.Action.Widget                 as Widget

import           Reactive.Commands.Command                           (Command, execCommand)
import           Reactive.State.Global                               (State)
import qualified Reactive.State.Global                               as Global

import qualified JS.UI                                               as UI
import           JS.WebSocket                                        (WebSocket)

import qualified Data.JSString     as JSString

foreign import javascript unsafe "console.time($1);"    consoleTimeStart' :: JSString.JSString -> IO ()
foreign import javascript unsafe "console.timeEnd($1);" consoleTimeEnd'   :: JSString.JSString -> IO ()

consoleTimeStart = consoleTimeStart' . JSString.pack
consoleTimeEnd   = consoleTimeEnd'   . JSString.pack

toTransformer :: Command a () -> (IO (), a) -> (IO (), a)
toTransformer cmd (_, a) = execCommand cmd a

actions =  [ Debug.toActionEv
           , Control.toAction
           , Widget.toAction
           , General.toAction
           , Camera.toAction
           , Graph.toAction
           , MultiSelection.toAction
           , Drag.toAction
           , Connect.toAction
           , NodeSearcher.toAction
           , ProjectManager.toAction
           , ConnectionPen.toAction
           , Sandbox.toAction
           , Debug.toAction
           ]

runCommands :: [Event.Event -> Maybe (Command State ())] -> Event.Event -> Command State ()
runCommands cmds event = sequence_ . catMaybes $ fmap ($ event) actions

preprocessEvent :: Event.Event -> IO Event.Event
preprocessEvent ev = do
    let batchEvent = BatchEventProcessor.process  ev
    customEvent   <- CustomEventProcessor.process ev

    return $ fromMaybe ev $ getLast $ Last batchEvent <> Last customEvent

processEvent :: MVar State -> Event.Event -> IO ()
processEvent var ev = do
    state     <- takeMVar var
    realEvent <- preprocessEvent ev
    consoleTimeStart $ (realEvent ^. Event.name)
    jsState   <- Handlers.getJSState
    let state' = state & Global.jsState .~ jsState
    let (ioActions, newState) = execCommand (runCommands actions realEvent) state'
    catch (ioActions >> UI.shouldRender) (handleExcept newState realEvent)
    consoleTimeEnd $ (realEvent ^. Event.name)
    putMVar var newState

makeNetworkDescription :: WebSocket -> MVar State -> IO ()
makeNetworkDescription conn state = do
    let handlers = [ Handlers.resizeHandler
                   , Handlers.mouseDownHandler
                   , Handlers.mouseUpHandler
                   , Handlers.mouseMovedHandler
                   , Handlers.mouseDblClickHandler
                   , Handlers.mouseWheelHandler
                   , Handlers.keyDownHandler
                   , Handlers.keyPressedHandler
                   , Handlers.keyUpHandler
                   , Handlers.webSocketHandler conn
                   , Handlers.connectionPenHandler
                   , Handlers.textEditorHandler
                   , Handlers.customEventHandler
                   ]

    let registerHandler (AddHandler rh) = rh (processEvent state)

    sequence_ $ registerHandler <$> handlers

handleExcept :: State -> Event.Event -> JSException  -> IO ()
handleExcept _ event except = do
    putStrLn $ "JavaScriptException: " <> (show except) <> "\n\nwhile processing: " <> (show event)
