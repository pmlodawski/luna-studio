module Reactive.Plugins.Core.Action.State.NodeSearcher where


import           Control.Lens
import           Data.Default
import           Data.Monoid

import           Object.Node
import           Utils.PrettyPrinter
import           GHCJS.Types        ( JSRef )

import qualified JS.NodeSearcher


data State = State { _isOpen :: Bool
                   } deriving (Eq, Show)

makeLenses ''State

instance Default State where
    def = State False

instance PrettyPrinter State where
    display (State True)   = "nsS(True )"
    display (State False)  = "nsS(False)"

