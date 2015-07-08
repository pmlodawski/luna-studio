module Reactive.Plugins.Core.Network where

import           Control.Applicative
import           Control.Monad              ( when )
import           Control.Lens
import           Data.Default
import           Data.Char                  ( chr )
import           Data.Monoid                ( (<>) )
import           Data.Maybe                 ( isJust, fromJust, catMaybes )
import           Data.Dynamic
import           Data.Traversable           ( sequenceA )

import           Reactive.Banana
import           Reactive.Banana.Frameworks
import           Reactive.Banana.Utils
import           Reactive.Handlers
import           JS.Bindings
import           Object.Object
import           Object.Dynamic             ( unpackDynamic )
import           Object.Node                ( Node(..) )
import qualified Object.Node    as Node
import qualified Event.Event    as Event
import qualified Event.Keyboard as Keyboard ( KeyMods(..), Event(..) )
import qualified Event.Mouse    as Mouse
import           Utils.PrettyPrinter

import           Reactive.Plugins.Core.Action.Action
import qualified Reactive.Plugins.Core.Action.Action        as Action
import qualified Reactive.Plugins.Core.Action.AddRemove     as AddRemove
import qualified Reactive.Plugins.Core.Action.Selection     as Selection
import qualified Reactive.Plugins.Core.Action.Drag          as Drag
import qualified Reactive.Plugins.Core.Action.Camera        as Camera
import qualified Reactive.Plugins.Core.Action.NodeSearcher  as NodeSearcher
import           Reactive.Plugins.Core.Action.Executor

import           Reactive.Plugins.Core.Action.State.Global



makeNetworkDescription :: forall t. Frameworks t => Moment t ()
makeNetworkDescription = do
    mouseDownE    <- fromAddHandler mouseDownHandler
    mouseUpE      <- fromAddHandler mouseUpHandler
    mouseMovedE   <- fromAddHandler mouseMovedHandler
    keyDownE      <- fromAddHandler keyDownHandler
    keyPressedE   <- fromAddHandler keyPressedHandler
    keyUpE        <- fromAddHandler keyUpHandler
    nodeSearcherE <- fromAddHandler nodeSearcherHander

    let
        anyE                         :: Event t (Event.Event Dynamic)
        anyE                          = unions [mouseDownE, mouseUpE, mouseMovedE, keyDownE, keyPressedE, keyUpE, nodeSearcherE]
        anyNodeE                     :: Event t (Event.Event Node)
        anyNodeE                      = unpackDynamic <$> anyE

        globalStateB                 :: Behavior t State
        globalStateB                  = stepper def $ globalStateReactionB <@ anyE

        nodeAddRemActionB             = fmap ActionST . stepper def $    AddRemove.toAction <$> anyNodeE
        nodeSelectionActionB          = fmap ActionST . stepper def $    Selection.toAction <$> anyNodeE
        nodeDragActionB               = fmap ActionST . stepper def $         Drag.toAction <$> anyNodeE
        cameraActionB                 = fmap ActionST . stepper def $       Camera.toAction <$> anyNodeE
        nodeSearcherActionB           = fmap ActionST . stepper def $ NodeSearcher.toAction <$> anyNodeE

        allActionsPackB               = [nodeAddRemActionB, nodeSelectionActionB, nodeDragActionB, cameraActionB, nodeSearcherActionB]

        (globalStateReactionB, allReactionsPackB) = execAll globalStateB allActionsPackB

        allReactionsSeqPackB         :: Behavior t [ActionUI]
        allReactionsSeqPackB          = sequenceA allReactionsPackB



    -- initial logB >>= liftIO . logAs ""

    allReactionsSeqPackF <- changes allReactionsSeqPackB
    reactimate' $ (fmap updatAllUI) <$> allReactionsSeqPackF
    reactimate' $ (fmap logAllUI)   <$> allReactionsSeqPackF

    -- nodeSelectionReactionF <- changes nodeSelectionReactionB
    -- reactimate' $ (fmap Selection.updateUI) <$> nodeSelectionReactionF
    -- reactimate' $ (fmap $ logIfActionAs "s|")       <$> nodeSelectionReactionF

    -- nodeDragReactionF <- changes nodeDragReactionB
    -- reactimate' $ (fmap Drag.updateUI) <$> nodeDragReactionF
    -- reactimate' $ (fmap $ logIfActionAs "d|")  <$> nodeDragReactionF

    -- nodeAddRemReactionF <- changes nodeAddRemReactionB
    -- reactimate' $ (fmap AddRemove.updateUI) <$> nodeAddRemReactionF
    -- reactimate' $ (fmap $ logIfActionAs "r|")       <$> nodeAddRemReactionF

    -- nodeSearcherReactionF <- changes nodeSearcherReactionB
    -- reactimate' $ (fmap NodeSearcher.updateUI) <$> nodeSearcherReactionF
    -- reactimate' $ (fmap $ logIfActionAs "r|")       <$> nodeSearcherReactionF


    -- globalStateReactionF <- changes globalStateReactionB
    -- reactimate' $ (fmap updateUI) <$> globalStateReactionF
    -- reactimate' $ (fmap $ logAll) <$> globalStateReactionF

    return ()
