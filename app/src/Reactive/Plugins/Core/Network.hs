module Reactive.Plugins.Core.Network where

import           Control.Applicative
import           Control.Lens
import           Data.Default
import           Data.Dynamic
import           Data.Traversable           ( sequenceA )

import           Reactive.Banana
import           Reactive.Banana.Frameworks
import           Reactive.Banana.Utils
import           Reactive.Handlers
import           JS.Appjs
import           Object.Object
import           Object.Dynamic             ( unpackDynamic )
import           Object.Node                ( Node(..) )
import qualified Object.Node                                 as Node
import qualified Event.Event                                 as Event
import qualified Event.Mouse                                 as Mouse
import           Utils.PrettyPrinter

import           Reactive.Plugins.Core.Action.Action
import qualified Reactive.Plugins.Core.Action.General        as General
import qualified Reactive.Plugins.Core.Action.Camera         as Camera
import qualified Reactive.Plugins.Core.Action.AddRemove      as AddRemove
import qualified Reactive.Plugins.Core.Action.Selection      as Selection
import qualified Reactive.Plugins.Core.Action.MultiSelection as MultiSelection
import qualified Reactive.Plugins.Core.Action.Drag           as Drag
import qualified Reactive.Plugins.Core.Action.NodeSearcher   as NodeSearcher
import           Reactive.Plugins.Core.Action.Executor

import           Reactive.Plugins.Core.Action.State.Global



makeNetworkDescription :: forall t. Frameworks t => Bool -> Moment t ()
makeNetworkDescription logging = do
    resizeE       <- fromAddHandler resizeHandler
    mouseDownE    <- fromAddHandler mouseDownHandler
    mouseUpE      <- fromAddHandler mouseUpHandler
    mouseMovedE   <- fromAddHandler mouseMovedHandler
    keyDownE      <- fromAddHandler keyDownHandler
    keyPressedE   <- fromAddHandler keyPressedHandler
    keyUpE        <- fromAddHandler keyUpHandler
    nodeSearcherE <- fromAddHandler nodeSearcherHander

    let
        anyE                         :: Event t (Event.Event Dynamic)
        anyE                          = unions [ resizeE
                                               , mouseDownE
                                               , mouseUpE
                                               , mouseMovedE
                                               , keyDownE
                                               , keyPressedE
                                               , keyUpE
                                               , nodeSearcherE
                                               ]
        anyNodeE                     :: Event t (Event.Event Node)
        anyNodeE                      = unpackDynamic <$> anyE
        anyNodeB                      = stepper def anyNodeE

        globalStateB                 :: Behavior t State
        globalStateB                  = stepper def $ globalStateReactionB <@ anyE

        nodesUnderCursorB             = nodesUnderCursor <$> globalStateB

        nodeGeneralActionB            = fmap ActionST $        General.toAction <$> anyNodeB
        cameraActionB                 = fmap ActionST $         Camera.toAction <$> anyNodeB
        nodeAddRemActionB             = fmap ActionST $      AddRemove.toAction <$> anyNodeB
        nodeSelectionActionB          = fmap ActionST $      Selection.toAction <$> anyNodeB <*> nodesUnderCursorB
        nodeMultiSelectionActionB     = fmap ActionST $ MultiSelection.toAction <$> anyNodeB <*> nodesUnderCursorB
        nodeDragActionB               = fmap ActionST $           Drag.toAction <$> anyNodeB <*> nodesUnderCursorB
        nodeSearcherActionB           = fmap ActionST $   NodeSearcher.toAction <$> anyNodeB

        allActionsPackB               = [ nodeGeneralActionB
                                        , nodeAddRemActionB
                                        , nodeSelectionActionB
                                        , nodeMultiSelectionActionB
                                        , nodeDragActionB
                                        , cameraActionB
                                        , nodeSearcherActionB
                                        ]

        (globalStateReactionB, allReactionsPackB) = execAll globalStateB allActionsPackB

        allReactionsSeqPackB         :: Behavior t [ActionUI]
        allReactionsSeqPackB          = sequenceA allReactionsPackB




    allReactionsSeqPackF <- changes allReactionsSeqPackB
    reactimate' $ (fmap updateAllUI) <$> allReactionsSeqPackF

    case logging of
        True  -> do
            reactimate' $ (fmap logAllUI) <$> allReactionsSeqPackF
            reactimate  $ (logAs "")      <$> anyE
        False -> return ()

    return ()
