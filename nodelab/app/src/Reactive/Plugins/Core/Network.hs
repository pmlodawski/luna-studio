module Reactive.Plugins.Core.Network where

import           Utils.PreludePlus

import           Control.Exception (catch)
import           GHCJS.Prim (JSException)

import           Reactive.Banana            (Event, Moment)
import qualified Reactive.Banana            as RB
import           Reactive.Banana.Frameworks (Frameworks, reactimate, fromAddHandler)
import qualified Reactive.Handlers          as Handlers

import qualified Event.Event                as Event
import qualified Event.Processors.Batch     as BatchEventProcessor

import qualified Reactive.Plugins.Core.Action.General                as General
import qualified Reactive.Plugins.Core.Action.Camera                 as Camera
import qualified Reactive.Plugins.Core.Action.MultiSelection         as MultiSelection
import qualified Reactive.Plugins.Core.Action.Drag                   as Drag
import qualified Reactive.Plugins.Core.Action.Connect                as Connect
import qualified Reactive.Plugins.Core.Action.NodeSearcher           as NodeSearcher
import qualified Reactive.Plugins.Core.Action.Widget                 as Widget
import qualified Reactive.Plugins.Core.Action.Backend.Backend        as Backend
import qualified Reactive.Plugins.Core.Action.Backend.Runner         as Runner
import qualified Reactive.Plugins.Core.Action.Backend.GraphFetcher   as GraphFetcher
import qualified Reactive.Plugins.Core.Action.Backend.Graph          as Graph
import qualified Reactive.Plugins.Core.Action.Backend.ProjectManager as ProjectManager
import qualified Reactive.Plugins.Core.Action.ConnectionPen          as ConnectionPen
import qualified Reactive.Plugins.Core.Action.TextEditor             as TextEditor
import qualified Reactive.Plugins.Core.Action.Debug                  as Debug
import qualified Reactive.Plugins.Core.Action.Sandbox                as Sandbox

import           Reactive.Commands.Command (Command, execCommand)
import           Reactive.State.Global     (State)
import qualified Reactive.State.Global     as Global

import           Batch.Workspace           (Workspace)
import           JS.WebSocket              (WebSocket)
import qualified JS.UI                     as UI

toTransformer :: Command a () -> (IO (), a) -> (IO (), a)
toTransformer cmd (_, a) = execCommand cmd a

makeNetworkDescription :: forall t. Frameworks t => WebSocket -> Bool -> State -> Moment t ()
makeNetworkDescription conn logging initialState = do
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

    primitiveE <- RB.unions <$> sequence (fromAddHandler <$> handlers)

    let
        batchE = RB.filterJust $ BatchEventProcessor.process <$> primitiveE
        anyE   = primitiveE `RB.union` batchE

        actions :: [Event.Event -> Maybe (Command State ())]
        actions =  [ Debug.toActionEv
                   , Widget.toAction
                   , General.toAction
                   , Camera.toAction
                   , Graph.toAction
                   , MultiSelection.toAction
                   , Drag.toAction
                   , Connect.toAction
                   , NodeSearcher.toAction
                   , Backend.toAction
                   , Runner.toAction
                   , GraphFetcher.toAction
                   , ProjectManager.toAction
                   , ConnectionPen.toAction
                   , TextEditor.toAction
                   , Sandbox.toAction
                   , Debug.toAction
                   ]

        commands :: Event t (Command State ())
        commands =  sequence_ . catMaybes <$> fmap (\e -> fmap ($ e) actions) anyE

        transformers :: Event t ((IO (), State) -> (IO (), State))
        transformers =  toTransformer <$> commands

        reactions :: Event t (IO (), State)
        reactions =  RB.accumE (return (), initialState) transformers


    let handleIOExcept (act, state) = catch (act >> UI.shouldRender) (handleExcept state)
    reactimate $ handleIOExcept <$> reactions

handleExcept :: State -> JSException  -> IO ()
handleExcept state except = do
    putStrLn $ "JavaScriptException: " <> (show except) <> "\n\n " <> (show $ state ^. Global.eventNum) <> " @ " <> (show $ state ^. Global.lastEvent)
