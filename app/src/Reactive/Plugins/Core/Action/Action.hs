module Reactive.Plugins.Core.Action.Action where

import           Data.Monoid          ( (<>) )
import           Data.Default
import           Data.Maybe           ( isJust )
import           Control.Lens

import           Utils.PrettyPrinter

data WithState act st = WithState { _action :: act
                                  , _state  :: st
                                  } deriving (Eq, Show)

type WithStateMaybe mact st = WithState (Maybe mact) st

makeLenses ''WithState

filterAction :: WithStateMaybe mact st -> Bool
filterAction (WithState mact st) = isJust mact

instance (Default act, Default st) => Default (WithState act st) where
    def = WithState def def

instance (PrettyPrinter act, PrettyPrinter st) => PrettyPrinter (WithState act st) where
    display (WithState action state) = "na( " <> display action <> " " <> display state <> " )"
