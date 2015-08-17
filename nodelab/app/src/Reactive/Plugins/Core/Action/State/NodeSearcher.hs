module Reactive.Plugins.Core.Action.State.NodeSearcher where


import           Utils.PreludePlus
import           Utils.Vector

import           Object.Object


data State = State { _isOpen :: Bool
                   } deriving (Eq, Show)

makeLenses ''State

instance Default State where
    def = State False

instance PrettyPrinter State where
    display (State True)   = "nsS(True )"
    display (State False)  = "nsS(False)"

