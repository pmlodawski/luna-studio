module Reactive.Plugins.Core.Network where

import           Utils.PreludePlus
import           Data.Dynamic

import           Reactive.Banana
import           Reactive.Banana.Frameworks
import           Reactive.Handlers
import qualified JS.NodeGraph                                as UI
import           Object.Object
import           Object.Dynamic             ( unpackDynamic )
import           Object.Node                ( Node(..) )
import qualified Object.Node                                 as Node
import qualified Event.Event                                 as Event
import qualified Event.Processors.Batch                      as BatchEventProcessor

import           Reactive.Plugins.Core.Action.Action
import qualified Reactive.Plugins.Core.Action.General              as General
import qualified Reactive.Plugins.Core.Action.Camera               as Camera
import qualified Reactive.Plugins.Core.Action.AddRemove            as AddRemove
import qualified Reactive.Plugins.Core.Action.Selection            as Selection
import qualified Reactive.Plugins.Core.Action.MultiSelection       as MultiSelection
import qualified Reactive.Plugins.Core.Action.Drag                 as Drag
import qualified Reactive.Plugins.Core.Action.Connect              as Connect
import qualified Reactive.Plugins.Core.Action.NodeSearcher         as NodeSearcher
import qualified Reactive.Plugins.Core.Action.Breadcrumb           as Breadcrumb
import qualified Reactive.Plugins.Core.Action.Widget               as Widget
import qualified Reactive.Plugins.Core.Action.Sandbox              as Sandbox
import qualified Reactive.Plugins.Core.Action.Backend.Backend      as Backend
import qualified Reactive.Plugins.Core.Action.Backend.Runner       as Runner
import qualified Reactive.Plugins.Core.Action.Backend.GraphFetcher as GraphFetcher
import           Reactive.Plugins.Core.Action.Executor

import           Reactive.Plugins.Core.Action.State.Global
import           Reactive.Plugins.Core.Action.State.UnderCursor

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
    keyDownE       <- fromAddHandler keyDownHandler
    keyPressedE    <- fromAddHandler keyPressedHandler
    keyUpE         <- fromAddHandler keyUpHandler
    nodeSearcherE  <- fromAddHandler nodeSearcherHander
    webSocketE     <- fromAddHandler $ webSocketHandler conn

    let
        batchE                       :: Event t (Event.Event Dynamic)
        batchE                        = filterJust $ BatchEventProcessor.process <$> webSocketE

        anyE                         :: Event t (Event.Event Dynamic)
        anyE                          = unions [ resizeE
                                               , mouseDownE
                                               , mouseUpE
                                               , mouseMovedE
                                               , mouseClickE
                                               , mouseDblClickE
                                               , keyDownE
                                               , keyPressedE
                                               , keyUpE
                                               , nodeSearcherE
                                               , batchE
                                               ]
        anyNodeE                     :: Event t (Event.Event Node)
        anyNodeE                      = unpackDynamic <$> anyE
        anyNodeB                      = stepper def anyNodeE

        globalStateB                 :: Behavior t State
        globalStateB                  = stepper (initialState workspace) $ globalStateReactionB <@ anyE

        underCursorB                 :: Behavior t UnderCursor
        underCursorB                  = underCursor <$> globalStateB

        widgetActionB                 = fmap ActionST $              Widget.toAction <$> anyNodeB
        nodeGeneralActionB            = fmap ActionST $             General.toAction <$> anyNodeB
        cameraActionB                 = fmap ActionST $              Camera.toAction <$> anyNodeB <*> globalStateB
        nodeAddRemActionB             = fmap ActionST $           AddRemove.toAction <$> anyNodeB <*> globalStateB
        nodeSelectionActionB          = fmap ActionST $           Selection.toAction <$> anyNodeB <*> globalStateB <*> underCursorB 
        nodeMultiSelectionActionB     = fmap ActionST $      MultiSelection.toAction <$> anyNodeB <*> globalStateB <*> underCursorB
        nodeDragActionB               = fmap ActionST $                Drag.toAction <$> anyNodeB <*> underCursorB
        nodeConnectActionB            = fmap ActionST $             Connect.toAction <$> anyNodeB <*> globalStateB
        nodeSearcherActionB           = fmap ActionST $        NodeSearcher.toAction <$> anyNodeB
        breadcrumbActionB             = fmap ActionST $          Breadcrumb.toAction <$> anyNodeB <*> globalStateB
        sandboxActionB                = fmap ActionST $             Sandbox.toAction <$> anyNodeB <*> globalStateB
        backendActionB                = fmap ActionST $             Backend.toAction <$> anyNodeB
        runnerActionB                 = fmap ActionST $              Runner.toAction <$> anyNodeB
        graphFetcherActionB           = fmap ActionST $        GraphFetcher.toAction <$> anyNodeB <*> globalStateB

        allActionsPackB               = [ nodeGeneralActionB
                                        , widgetActionB
                                        , nodeAddRemActionB
                                        , nodeSelectionActionB
                                        , nodeMultiSelectionActionB
                                        , nodeDragActionB
                                        , cameraActionB
                                        , nodeConnectActionB
                                        , nodeSearcherActionB
                                        , breadcrumbActionB
                                        , sandboxActionB
                                        , backendActionB
                                        , runnerActionB
                                        , graphFetcherActionB
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
