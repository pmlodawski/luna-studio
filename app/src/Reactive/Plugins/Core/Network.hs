module Reactive.Plugins.Core.Network where

import           Control.Applicative
import           Control.Monad              ( when )
import           Control.Lens
import           Data.Default
import           Data.Char                  ( chr )
import           Data.Monoid                ( (<>) )
import           Data.Maybe                 ( isJust, fromJust, catMaybes )
import           Data.Dynamic
import           Debug.Trace                ( trace )

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
import           Event.Mouse                ( WithObjects(..) )
import qualified Event.Mouse    as Mouse
import           Utils.PrettyPrinter

import qualified Reactive.Plugins.Core.Action.Action    as Action
import qualified Reactive.Plugins.Core.Action.AddRemove as AddRemove
import qualified Reactive.Plugins.Core.Action.Selection as Selection
import qualified Reactive.Plugins.Core.Action.Drag      as Drag

import           Reactive.Plugins.Core.Action.State.Global


-- data NewState = NewState String deriving (Show)
-- instance PrettyPrinter NewState where
    -- display = show


makeNetworkDescription :: forall t. Frameworks t => Moment t ()
makeNetworkDescription = do
    mouseDownE    <- fromAddHandler mouseDownHandler
    mouseUpE      <- fromAddHandler mouseUpHandler
    mouseMovedE   <- fromAddHandler mouseMovedHandler
    keyPressedE   <- fromAddHandler keyPressedHandler

    let
        -- aaaB = accumB (NewState "") $ (\(NewState c) (NewState o) -> NewState $ "a(" <> c <> ")") <$> (cccB <@ keyPressedE)
        -- bbbB = accumB (NewState "") $ (\(NewState a) (NewState o) -> NewState $ "b(" <> a <> ")") <$> (aaaB <@ keyPressedE)
        -- cccB = accumB (NewState "") $ (\(NewState b) (NewState o) -> NewState $ "c(" <> b <> ")") <$> (bbbB <@ keyPressedE)

        anyE       = unions [() <$ mouseDownE, () <$ mouseUpE, () <$ mouseMovedE, () <$ keyPressedE]
        anyNoMoveE = unions [() <$ mouseDownE, () <$ mouseUpE, () <$ keyPressedE]


        globalStateB                  = stepper def $ globalStateReactionB <@ anyE

        -- nodeSelectionStateB           = stepper def $ nodeSelectionReactionStateB <@ anyE
        -- nodeDragStateB                = stepper def $ nodeDragReactionStateB      <@ anyE

        -- lastKeyB                      = stepper def $ Just <$> keyPressedE

        mouseNodeDownE, mouseNodeUpE, mouseNodeMovedE :: Event t (WithObjects Node)
        mouseNodeDownE                = unpackDynamic <$> mouseDownE
        mouseNodeUpE                  = unpackDynamic <$> mouseUpE
        mouseNodeMovedE               = unpackDynamic <$> mouseMovedE

        -- mouseE                     = mouseDownE <+> mouseUpE <+> mouseMovedE
        mouseNodeE                    = unions [mouseNodeDownE, mouseNodeUpE, mouseNodeMovedE]

        mouseNodeSelectActionE        = Selection.mouseToAction      <$> mouseNodeE
        keyboardNodeSelectionActionE  = Selection.keyboardToAction   <$> keyPressedE
        nodeSelectionActionE          = unions [mouseNodeSelectActionE, keyboardNodeSelectionActionE]
        nodeSelectionActionB          = stepper def $ nodeSelectionActionE

        nodeSelectionReactionB        = Action.tryExec  <$> nodeSelectionActionB <*> globalStateB
        -- nodeSelectionReactionStateB   = Action.getState <$> nodeSelectionReactionB

        globalStateReactionB   = Action.getState <$> nodeSelectionReactionB



        -- nodeDragActionE               = Drag.mouseToAction           <$> mouseNodeE
        -- nodeDragActionB               = stepper def $ nodeDragActionE
        -- -- -- nodeDragReactionB            :: Behavior t (Action.WithState (Maybe Drag.Action) Drag.State)
        -- nodeDragReactionB             = Action.tryExec  <$> nodeDragActionB <*> nodeDragStateB
        -- -- -- nodeDragReactionStateB       :: Behavior t Drag.State
        -- nodeDragReactionStateB        = Action.getState <$> nodeDragReactionB




        -- nodeDragSelectionE            = Drag.AccumInput <$> nodeSelectionB        <@> nodeDragActionsE
        -- nodeDragReactionsE            = filterE Action.filterAction $ accumE def $ Drag.accumActionState <$> nodeDragSelectionE


        -- nodeSelectionReactionE        = accumE def $ Selection.execActionOnState   <$> nodeSelectionActionE

        -- nodeSelectionB                = stepper def $ Selection.toNodeSelection   <$> nodeSelectionReactionE

        -- nodeDragActionsE              = filterJust $ Drag.mouseToAction           <$> mouseNodeE
        -- nodeDragSelectionE            = Drag.AccumInput <$> nodeSelectionB        <@> nodeDragActionsE
        -- nodeDragReactionsE            = filterE Action.filterAction $ accumE def $ Drag.accumActionState <$> nodeDragSelectionE


    -- initial logB >>= liftIO . logAs ""

    -- aaaF <- changes aaaB
    -- bbbF <- changes bbbB
    -- cccF <- changes cccB
    -- reactimate' $ (fmap $ logAs "A: ") <$> aaaF
    -- reactimate' $ (fmap $ logAs "B: ") <$> bbbF
    -- reactimate' $ (fmap $ logAs "C: ") <$> cccF

    -- mouseNodeReactionF <- changes mouseNodeReactionB
    -- reactimate' $ (fmap Node.updateNodes) <$> mouseNodeReactionF

    nodeSelectionReactionF <- changes nodeSelectionReactionB
    reactimate' $ (fmap Selection.updateUI) <$> nodeSelectionReactionF
    reactimate' $ (fmap $ logAs "s|")       <$> nodeSelectionReactionF

    -- nodeDragReactionF <- changes nodeDragReactionB
    -- reactimate' $ (fmap Drag.updateUI) <$> nodeDragReactionF
    -- reactimate' $ (fmap $ logAs "d|")  <$> nodeDragReactionF



    -- reactimate $ Selection.updateUI <$> nodeSelectionReactionE
    -- reactimate $ Drag.updateUI      <$> nodeDragReactionsE

    -- reactimate $ logAs "n: " <$> mouseNodeE
    -- reactimate $ logAs "s: " <$> nodeSelectionReactionE
    -- reactimate $ logAs "d: " <$> nodeDragReactionsE

    -- nodeDragReactionsF <- changes nodeDragReactionsB
    -- reactimate' $ (fmap Drag.updateUI) <$> nodeDragReactionsF
    -- reactimate' $ (fmap $ logAs "d: ") <$> ((fmap display) <$> nodeDragReactionsF)

    return ()
