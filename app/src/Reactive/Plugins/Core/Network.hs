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
import qualified Event.Mouse    as Mouse
import           Utils.PrettyPrinter

import qualified Reactive.Plugins.Core.Action.Action    as Action
import qualified Reactive.Plugins.Core.Action.AddRemove as AddRemove
import qualified Reactive.Plugins.Core.Action.Selection as Selection
import qualified Reactive.Plugins.Core.Action.Drag      as Drag
import qualified Reactive.Plugins.Core.Action.Camera    as Camera
-- import qualified Reactive.Plugins.Core.Action.Executor  as Executor
import qualified Reactive.Plugins.Core.Action.NodeSearcher  as NodeSearcher
-- import qualified Reactive.Plugins.Core.Action.Executor  as Executor
import           Reactive.Plugins.Core.Action.Executor

import           Reactive.Plugins.Core.Action.State.Global

import Data.Traversable (sequenceA)

updateUI :: ( State
            , Action.WithStateMaybe AddRemove.Action State
            , Action.WithStateMaybe Selection.Action State
            , Action.WithStateMaybe Drag.Action State
            , Action.WithStateMaybe Camera.Action State
            , Action.WithStateMaybe NodeSearcher.Action State
            ) -> IO ()
updateUI (_, addRem, sel, drag, cam, ns) = do
    AddRemove.updateUI       addRem
    Selection.updateUI       sel
    Drag.updateUI            drag
    Camera.updateUI          cam
    NodeSearcher.updateUI    ns


logAll :: ( State
            , Action.WithStateMaybe AddRemove.Action State
            , Action.WithStateMaybe Selection.Action State
            , Action.WithStateMaybe Drag.Action State
            , Action.WithStateMaybe Camera.Action State
            , Action.WithStateMaybe NodeSearcher.Action State
            ) -> IO ()
logAll (st, addRem, sel, drag, cam, ns) = do
    logAs "g|" st


logIfActionAs as ws = if isJust (ws ^. Action.action) then logAs as ws else return ()


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
        anyE                          = unions [mouseDownE, mouseUpE, mouseMovedE, keyDownE, keyPressedE, keyUpE, nodeSearcherE]

        globalStateB                 :: Behavior t State
        -- globalStateB                  = stepper def $ ((view _1) <$> globalStateReactionB) <@ anyE
        globalStateB                  = stepper def $ globalStateReactionB <@ anyE



        anyNodeE                     :: Event t (Event.Event Node)
        anyNodeE                      = unpackDynamic <$> anyE

        nodeAddRemActionE             = AddRemove.toAction <$> anyNodeE
        nodeAddRemActionB             = stepper def $ nodeAddRemActionE
        -- nodeAddRemReactionB           = Action.tryExec  <$> nodeAddRemActionB <*> globalStateB
        -- nodeAddRemReactionStateB      = Action.getState <$> nodeAddRemReactionB

        nodeSelectionActionE          = Selection.toAction <$> anyNodeE
        nodeSelectionActionB          = stepper def $ nodeSelectionActionE
        -- nodeSelectionReactionB        = Action.tryExec  <$> nodeSelectionActionB <*> globalStateB
        -- nodeSelectionReactionStateB   = Action.getState <$> nodeSelectionReactionB

        nodeDragActionE               = Drag.toAction <$> anyNodeE
        nodeDragActionB               = stepper def $ nodeDragActionE
        -- nodeDragReactionB             = Action.tryExec  <$> nodeDragActionB <*> globalStateB
        -- nodeDragReactionStateB        = Action.getState <$> nodeDragReactionB

        cameraActionE                 = Camera.toAction <$> anyNodeE
        cameraActionB                 = stepper def $ cameraActionE

        nodeSearcherActionE           = NodeSearcher.toAction <$> anyNodeE
        nodeSearcherActionB           = stepper def $ nodeSearcherActionE
        -- nodeDragActionB :: Int

        nodeAddRemReactionB           = Action.tryExec  <$> nodeAddRemActionB    <*> globalStateB
        nodeSelectionReactionB        = Action.tryExec  <$> nodeSelectionActionB <*> globalStateB
        nodeDragReactionB             = Action.tryExec  <$> nodeDragActionB      <*> globalStateB
        nodeSearcherReactionB         = Action.tryExec  <$> nodeSearcherActionB  <*> globalStateB

        -- ss1B :: Int
        -- ss1B = (Action.pureAction) <$> nodeAddRemActionB
        -- ss1B :: Behavior t (forall act. Action.ActionStateExecutor (Maybe act) State => [Maybe act])

        -- ss2B = (Action.appendAction) <$> nodeSelectionActionB <*> ss1B
        -- -- ss2B :: Behavior t (forall act. Action.ActionStateExecutor act State => [act])

        -- gB = Executor.execAll2 <$> globalStateB <*> ss2B
        -- gB :: forall act. Action.ActionStateExecutor act State => [Action.WithState (Maybe act) State]

        -- nodeAddRemActionB :: Int

        -- globalStateReactionB :: Behavior t ( State
        --                                    , Action.WithStateMaybe AddRemove.Action State
        --                                    , Action.WithStateMaybe Selection.Action State
        --                                    , Action.WithStateMaybe Drag.Action State
        --                                    , Action.WithStateMaybe Camera.Action State
        --                                    )
        -- globalStateReactionB           = Executor.execAll <$> globalStateB
        --                                                   <*> nodeAddRemActionB
        --                                                   <*> nodeSelectionActionB
        --                                                   <*> nodeDragActionB
        --                                                   <*> cameraActionB

        -- globalStateReactionB = Executor.execAll3 globalStateB [ nodeAddRemActionB
        --                                      , nodeSelectionActionB
        --                                      , nodeDragActionB
        --                                      , cameraActionB
        --                                      ]


        globalStateReactionB :: Behavior t State
        globalStateReactionB           = execAll' <$> globalStateB
                                                           <*> nodeAddRemActionB
                                                           <*> nodeSelectionActionB
                                                           <*> nodeDragActionB
                                                           <*> cameraActionB
                                                           <*> nodeSearcherActionB

        -- foo = Executor.execAll2 globalStateB [ nodeAddRemActionB
        --                                      , nodeSelectionActionB
        --                                      , nodeDragActionB
        --                                      , cameraActionB
        --                                      ]

        -- lst =


        -- x = Executor.execAll2 globalStateB [nodeAddRemActionB, nodeSelectionActionB]

        -- y = Executor.ActionList [nodeAddRemActionB, nodeSelectionActionB]
        -- s = sequenceA [ nodeAddRemActionB
        --               , nodeSelectionActionB
        --               ]


        -- x = Executor.execAll2 <$> globalStateB <*> s


    -- initial logB >>= liftIO . logAs ""

    nodeSelectionReactionF <- changes nodeSelectionReactionB
    reactimate' $ (fmap Selection.updateUI) <$> nodeSelectionReactionF
    reactimate' $ (fmap $ logIfActionAs "s|")       <$> nodeSelectionReactionF

    nodeDragReactionF <- changes nodeDragReactionB
    reactimate' $ (fmap Drag.updateUI) <$> nodeDragReactionF
    reactimate' $ (fmap $ logIfActionAs "d|")  <$> nodeDragReactionF

    nodeAddRemReactionF <- changes nodeAddRemReactionB
    reactimate' $ (fmap AddRemove.updateUI) <$> nodeAddRemReactionF
    reactimate' $ (fmap $ logIfActionAs "r|")       <$> nodeAddRemReactionF

    nodeSearcherReactionF <- changes nodeSearcherReactionB
    reactimate' $ (fmap NodeSearcher.updateUI) <$> nodeSearcherReactionF
    reactimate' $ (fmap $ logIfActionAs "r|")       <$> nodeSearcherReactionF


    -- globalStateReactionF <- changes globalStateReactionB
    -- reactimate' $ (fmap updateUI) <$> globalStateReactionF
    -- reactimate' $ (fmap $ logAll) <$> globalStateReactionF

    return ()
