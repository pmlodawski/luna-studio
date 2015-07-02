module Reactive.Plugins.Core.Action.Action where

import           Data.Monoid          ( (<>) )
import           Data.Default
import           Data.Maybe           ( isJust )
import           Data.Functor
import           Control.Lens

import           Utils.PrettyPrinter

data WithState act st = WithState { _action :: act
                                  , _state  :: st
                                  } deriving (Eq, Show)

type WithStateMaybe act st = WithState (Maybe act) st

makeLenses ''WithState

getState :: WithState act st -> st
getState = (^. state)

filterAction :: WithStateMaybe mact st -> Bool
filterAction (WithState mact st) = isJust mact

instance (Default act, Default st) => Default (WithState act st) where
    def = WithState def def

instance (PrettyPrinter act, PrettyPrinter st) => PrettyPrinter (WithState act st) where
    display (WithState action state) = "na( " <> display action <> " " <> display state <> " )"


class ActionStateExecutor act st where
    exec    ::        act  -> st -> WithStateMaybe act st
    tryExec :: (Maybe act) -> st -> WithStateMaybe act st
    tryExec Nothing       = WithState Nothing
    tryExec (Just action) = exec action

