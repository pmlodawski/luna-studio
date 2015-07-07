module Reactive.Plugins.Core.Action.Action where

import           Data.Monoid          ( (<>) )
import           Data.Default
import           Data.Maybe           ( isJust )
import           Data.Functor
import           Control.Lens

import           Reactive.Plugins.Core.Action.State.Global
import           Utils.PrettyPrinter

data WithState act st = WithState { _action :: act
                                  , _state  :: st
                                  } -- deriving (Show)

type WithStateMaybe act st = WithState (Maybe act) st

makeLenses ''WithState

-- getState :: WithState act st -> st
-- getState = _state

-- filterAction :: WithStateMaybe mact st -> Bool
-- filterAction (WithState mact st) = isJust mact

instance (Default act, Default st) => Default (WithState act st) where
    def = WithState def def

instance (PrettyPrinter act, PrettyPrinter st) => PrettyPrinter (WithState act st) where
    display (WithState action state) = "na( " <> display action <> " " <> display state <> " )"


class ActionStateExecutor act st where
    exec    ::        act  -> st -> WithStateMaybe act st
    tryExec :: (Maybe act) -> st -> WithStateMaybe act st
    tryExec Nothing       = WithState Nothing
    tryExec (Just action) = exec action

    -- pureAction :: forall act. ActionStateExecutor act st => act -> [act]
    -- pureAction a = [a]

    -- appendAction :: forall act. ActionStateExecutor act st => act -> [act] -> [act]
    -- appendAction = (:)



class ActionStateUpdate act st where
    exec2    ::        act  -> st -> st
    tryExec2 :: (Maybe act) -> st -> st
    tryExec2 Nothing       = id
    tryExec2 (Just action) = exec2 action


class (ActionStateExecutor act State) => ActionGlobalStateExecutor act where


