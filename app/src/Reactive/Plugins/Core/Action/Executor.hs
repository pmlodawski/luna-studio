{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeSynonymInstances #-}


module Reactive.Plugins.Core.Action.Executor where

import           Data.Monoid          ( (<>) )
import           Data.Default
import           Data.Maybe           ( isJust )
import           Data.Functor
import           Control.Lens
import           Utils.PrettyPrinter

import           Reactive.Banana
import           Reactive.Banana.Frameworks
import           Reactive.Banana.Utils
import           Reactive.Handlers

import           Reactive.Plugins.Core.Action.Action
import qualified Reactive.Plugins.Core.Action.AddRemove as AddRemove
import qualified Reactive.Plugins.Core.Action.Selection as Selection
import qualified Reactive.Plugins.Core.Action.Drag      as Drag
import qualified Reactive.Plugins.Core.Action.Camera    as Camera
import           Reactive.Plugins.Core.Action.State.Global



execAll :: State -> Maybe AddRemove.Action
                 -> Maybe Selection.Action
                 -> Maybe Drag.Action
                 -> Maybe Camera.Action
                 -> ( State
                    , WithStateMaybe AddRemove.Action State
                    , WithStateMaybe Selection.Action State
                    , WithStateMaybe Drag.Action State
                    , WithStateMaybe Camera.Action State
                    )
execAll initState addRem selection drag cam = (finalState, wsAddRem, wsSel, wsDrag, wsCam)
    where
    wsAddRem   = tryExec addRem              initState
    wsSel      = tryExec selection $ wsAddRem ^. state
    wsDrag     = tryExec drag      $ wsSel    ^. state
    wsCam      = tryExec cam       $ wsDrag   ^. state
    finalState = wsCam                        ^. state


execAll' :: State -> Maybe AddRemove.Action
                 -> Maybe Selection.Action
                 -> Maybe Drag.Action
                 -> Maybe Camera.Action
                 -> State

execAll' initState addRem selection drag cam = finalState
    where
    wsAddRem   = tryExec addRem              initState
    wsSel      = tryExec selection $ wsAddRem ^. state
    wsDrag     = tryExec drag      $ wsSel    ^. state
    wsCam      = tryExec cam       $ wsDrag   ^. state
    finalState = wsCam                        ^. state


-- class ActionStateExecutor act st where
--     exec    ::        act  -> st -> WithStateMaybe act st
--     tryExec :: (Maybe act) -> st -> WithStateMaybe act st
--     tryExec Nothing       = WithState Nothing
--     tryExec (Just action) = exec action



-- <*> :: f (a -> b) -> f a -> f b

-- <*>    B (a -> [a]) -> f a -> f [a]

infixr 9 :-:
data ActionExecList = forall a. ActionStateExecutor a State => a :-: ActionExecList
                    | ActionExecNil

-- execAll2 :: forall act. ActionStateExecutor act State => State -> [Maybe act] -> [WithStateMaybe act State]
-- execAll2 stInit actions = scanl execAction (WithState Nothing stInit) actions
--     where
--     -- execAction :: WithStateMaybe act State -> Maybe act -> WithStateMaybe act State
--     execAction ws act = tryExec act $ ws ^. state


-- execAll2 :: State -> (forall act. (ActionStateExecutor act State) => [Maybe act] -> [WithStateMaybe act State])
-- execAll2 stInit actions = undefined where -- scanl execAction (WithState Nothing stInit) actions
--     -- execAction ws act = tryExec act $ ws ^. state


-- execAll2 :: State -> [forall act. (ActionStateExecutor act State) => Maybe act] -> State
execAll2 :: Behavior t State -> [forall act. (ActionStateExecutor act State) => Behavior t (Maybe act)] -> Behavior t State
execAll2 stInit actions = undefined where -- scanl execAction (WithState Nothing stInit) actions
    -- execAction ws act = tryExec act $ ws ^. state


data ActionList t = forall act. (ActionStateExecutor act State) => ActionList [Behavior t (Maybe act)]

-- instance Monoid (ActionList t) where
--     mempty = ActionList []
--     -- (ActionList (a:as)) `mappend` (ActionList a') = ActionList (a : a')

appendAction :: (forall act. (ActionStateExecutor act State) => Behavior t (Maybe act)) -> ActionList  t -> ActionList t
appendAction a (ActionList as) = ActionList (a:as)

pureAction :: (forall act. (ActionStateExecutor act State) => Behavior t (Maybe act)) -> ActionList t
pureAction a = ActionList [a]


-- appendAction :: forall act. ActionStateExecutor act State => act -> [act] -> [act]
-- appendAction = (:)





-- scanl :: (b -> a -> b) -> b -> [a] -> [b]


-- scanl f z [x1, x2, ...] == [z, z `f` x1, (z `f` x1) `f` x2, ...]


-- class SuperAction a where
--     trans :: st -> a -> st
--     upUI  :: a -> IO ()

-- appendSuperAction :: forall a. SuperAction a => a -> [a] -> [a]
-- appendSuperAction = (:)

-- instance SuperAction (WithStateMaybe AddRemove.Action State) where

-- appendSuperAction x xs = x : xs

-- -- act - typeclass

-- execList :: State -> [Maybe act]

-- execAll state list =

-- appendSuperAction nodeAddRemActionB
-- $ appendSuperAction nodeSelectionActionB
-- $ appendSuperAction nodeDragActionB
-- $ []
