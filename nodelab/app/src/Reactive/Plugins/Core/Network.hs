module Reactive.Plugins.Core.Network where

import           Utils.PreludePlus

import           Reactive.Banana
import           Reactive.Banana.Frameworks
import           Reactive.Handlers
import qualified JS.NodeGraph                                       as UI
import           Object.Object
import           Object.Node                ( Node(..) )
import qualified Object.Node                                        as Node
import qualified Event.Event                                        as Event
import qualified Event.Processors.Batch                             as BatchEventProcessor

import           Reactive.Plugins.Core.Action
import qualified Reactive.Plugins.Core.Action.General               as General
import qualified Reactive.Plugins.Core.Action.Camera                as Camera
import qualified Reactive.Plugins.Core.Action.MultiSelection        as MultiSelection
import qualified Reactive.Plugins.Core.Action.Drag                  as Drag
import qualified Reactive.Plugins.Core.Action.Connect               as Connect
import qualified Reactive.Plugins.Core.Action.NodeSearcher          as NodeSearcher
import qualified Reactive.Plugins.Core.Action.Widget                as Widget
import qualified Reactive.Plugins.Core.Action.Sandbox               as Sandbox
import qualified Reactive.Plugins.Core.Action.Backend.Backend       as Backend
import qualified Reactive.Plugins.Core.Action.Backend.Runner        as Runner
import qualified Reactive.Plugins.Core.Action.Backend.GraphFetcher  as GraphFetcher
import qualified Reactive.Plugins.Core.Action.Backend.AddNode       as AddNode
import qualified Reactive.Plugins.Core.Action.ConnectionPen         as ConnectionPen
import qualified Reactive.Plugins.Core.Action.TextEditor            as TextEditor
import           Reactive.Plugins.Core.Executor

import           Reactive.State.Global
import           Reactive.State.UnderCursor

import           JS.WebSocket (WebSocket)

import           Batch.Workspace

makeNetworkDescription :: forall t. Frameworks t => WebSocket -> Bool -> Workspace -> Moment t ()
makeNetworkDescription conn logging workspace = do
    resizeE        <- fromAddHandler resizeHandler
    mouseDownE     <- fromAddHandler mouseDownHandler
    mouseUpE       <- fromAddHandler mouseUpHandler
    mouseMovedE    <- fromAddHandler mouseMovedHandler
    mouseClickE    <- fromAddHandler mouseClickHandler
    mouseDblClickE <- fromAddHandler mouseDblClickHandler
    mouseWheeelE   <- fromAddHandler mouseWheelHandler
    keyDownE       <- fromAddHandler keyDownHandler
    keyPressedE    <- fromAddHandler keyPressedHandler
    keyUpE         <- fromAddHandler keyUpHandler
    nodeSearcherE  <- fromAddHandler nodeSearcherHander
    webSocketE     <- fromAddHandler $ webSocketHandler conn
    connectionPenE <- fromAddHandler connectionPenHandler
    textEditorE    <- fromAddHandler textEditorHandler

    let
        batchE                       :: Event t Event.Event
        batchE                        = filterJust $ BatchEventProcessor.process <$> webSocketE

        anyE                         :: Event t Event.Event
        anyE                          = unions [ resizeE
                                               , mouseDownE
                                               , mouseUpE
                                               , mouseMovedE
                                               , mouseClickE
                                               , mouseDblClickE
                                               , mouseWheeelE
                                               , keyDownE
                                               , keyPressedE
                                               , keyUpE
                                               , nodeSearcherE
                                               , batchE
                                               , connectionPenE
                                               , textEditorE
                                               ]

        anyNodeB                      = stepper def anyE

        globalStateB                 :: Behavior t State
        globalStateB                  = stepper (initialState workspace) $ globalStateReactionB <@ anyE

        underCursorB                 :: Behavior t UnderCursor
        underCursorB                  = underCursor <$> globalStateB

        widgetActionB                 = fmap ActionST $              Widget.toAction <$> anyNodeB
        nodeGeneralActionB            = fmap ActionST $             General.toAction <$> anyNodeB
        cameraActionB                 = fmap ActionST $              Camera.toAction <$> anyNodeB
        nodeAddActionB                = fmap ActionST $             AddNode.toAction <$> anyNodeB <*> globalStateB
        nodeMultiSelectionActionB     = fmap ActionST $      MultiSelection.toAction <$> anyNodeB
        nodeDragActionB               = fmap ActionST $                Drag.toAction <$> anyNodeB <*> globalStateB <*> underCursorB
        nodeConnectActionB            = fmap ActionST $             Connect.toAction <$> anyNodeB <*> globalStateB
        nodeSearcherActionB           = fmap ActionST $        NodeSearcher.toAction <$> anyNodeB
        sandboxActionB                = fmap ActionST $             Sandbox.toAction <$> anyNodeB <*> globalStateB
        backendActionB                = fmap ActionST $             Backend.toAction <$> anyNodeB
        runnerActionB                 = fmap ActionST $              Runner.toAction <$> anyNodeB
        graphFetcherActionB           = fmap ActionST $        GraphFetcher.toAction <$> anyNodeB <*> globalStateB
        connectionPenActionB          = fmap ActionST $       ConnectionPen.toAction <$> anyNodeB <*> globalStateB
        textEditorActionB             = fmap ActionST $          TextEditor.toAction <$> anyNodeB

        allActionsPackB               = [ nodeGeneralActionB
                                        , widgetActionB
                                        , nodeAddActionB
                                        , nodeMultiSelectionActionB
                                        , nodeDragActionB
                                        , cameraActionB
                                        , nodeConnectActionB
                                        , nodeSearcherActionB
                                        , sandboxActionB
                                        , backendActionB
                                        , runnerActionB
                                        , graphFetcherActionB
                                        , connectionPenActionB
                                        , textEditorActionB
                                        ]

        (globalStateReactionB, allReactionsPackB) = execAll globalStateB allActionsPackB

        allReactionsSeqPackB         :: Behavior t [ActionUI]
        allReactionsSeqPackB          = sequenceA allReactionsPackB


    allReactionsSeqPackF <- changes allReactionsSeqPackB
    reactimate' $ (fmap updateAllUI) <$> allReactionsSeqPackF

    case logging of
        True  -> do
            reactimate' $ (fmap logAllUI) <$> allReactionsSeqPackF
            reactimate  $ (UI.logAs "")   <$> anyE
        False -> return ()

    return ()
