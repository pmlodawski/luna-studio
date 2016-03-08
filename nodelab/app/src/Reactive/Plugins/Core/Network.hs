module Reactive.Plugins.Core.Network where

import           Utils.PreludePlus

import           Control.Exception (catch)
import           GHCJS.Prim (JSException)

import           Reactive.Handlers          (AddHandler(..))
import qualified Reactive.Handlers          as Handlers

import qualified Event.Event                as Event
import qualified Event.Processors.Batch     as BatchEventProcessor

import qualified Reactive.Plugins.Core.Action.General                as General
import qualified Reactive.Plugins.Core.Action.Camera                 as Camera
import qualified Reactive.Plugins.Core.Action.UILayout               as UILayout
import qualified Reactive.Plugins.Core.Action.MultiSelection         as MultiSelection
import qualified Reactive.Plugins.Core.Action.Drag                   as Drag
import qualified Reactive.Plugins.Core.Action.Connect                as Connect
import qualified Reactive.Plugins.Core.Action.NodeSearcher           as NodeSearcher
import qualified Reactive.Plugins.Core.Action.Widget                 as Widget
import qualified Reactive.Plugins.Core.Action.Backend.Control        as Control
import qualified Reactive.Plugins.Core.Action.Backend.Graph          as Graph
import qualified Reactive.Plugins.Core.Action.Backend.ProjectManager as ProjectManager
import qualified Reactive.Plugins.Core.Action.ConnectionPen          as ConnectionPen
import qualified Reactive.Plugins.Core.Action.TextEditor             as TextEditor
import qualified Reactive.Plugins.Core.Action.Debug                  as Debug
import qualified Reactive.Plugins.Core.Action.Sandbox                as Sandbox

import           Reactive.Commands.Command (Command, execCommand, performIO)
import           Reactive.State.Global     (State)
import qualified Reactive.State.Global     as Global

import           Batch.Workspace           (Workspace)
import           JS.WebSocket              (WebSocket)
import qualified JS.UI                     as UI

import Control.Concurrent.MVar
import Debug.Trace (trace)

toTransformer :: Command a () -> (IO (), a) -> (IO (), a)
toTransformer cmd (_, a) = execCommand cmd a


actions =  [ Debug.toActionEv
           , Control.toAction
           , Widget.toAction
           , General.toAction
           , Camera.toAction
           , UILayout.toAction
           , Graph.toAction
           , MultiSelection.toAction
           , Drag.toAction
           , Connect.toAction
           , NodeSearcher.toAction
           , ProjectManager.toAction
           , ConnectionPen.toAction
           , TextEditor.toAction
           , Sandbox.toAction
           , Debug.toAction
           ]

runCommands :: [Event.Event -> Maybe (Command State ())] -> Event.Event -> Command State ()
runCommands cmds event = sequence_ . catMaybes $ fmap ($ event) actions

preprocessBatchEvent :: Event.Event -> Event.Event
preprocessBatchEvent ev = fromMaybe ev $ BatchEventProcessor.process ev

processEvent :: MVar State -> Event.Event -> IO ()
processEvent var ev = do
    let realEvent = preprocessBatchEvent ev
    state <- takeMVar var
    let (ioActions, newState) = execCommand (runCommands actions realEvent) state
    catch (ioActions >> UI.shouldRender) (handleExcept newState realEvent)
    putMVar var newState



makeNetworkDescription :: WebSocket -> Bool -> MVar State -> IO ()
makeNetworkDescription conn logging state = do
    let handlers = [ Handlers.resizeHandler
                   , Handlers.mouseDownHandler
                   , Handlers.mouseUpHandler
                   , Handlers.mouseMovedHandler
                   , Handlers.mouseDblClickHandler
                   , Handlers.mouseWheelHandler
                   , Handlers.keyDownHandler
                   , Handlers.keyPressedHandler
                   , Handlers.keyUpHandler
                   , Handlers.nodeSearcherHander
                   , Handlers.webSocketHandler conn
                   , Handlers.connectionPenHandler
                   , Handlers.textEditorHandler
                   , Handlers.debugHandler
                   ]

    let registerHandler (AddHandler rh) = rh (processEvent state)

    sequence_ $ registerHandler <$> handlers

handleExcept :: State -> Event.Event -> JSException  -> IO ()
handleExcept state event except = do
    putStrLn $ "JavaScriptException: " <> (show except) <> "\n\nwhile processing: " <> (show event)
