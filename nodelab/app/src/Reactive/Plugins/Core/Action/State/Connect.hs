module Reactive.Plugins.Core.Action.State.Connect where


import           Utils.PreludePlus
import           Utils.Vector

import           Object.Node

data DragHistory = DragHistory { _dragStartPos    :: Vector2 Int
                               , _dragCurrentPos  :: Vector2 Int
                               } deriving (Eq, Show)

data Connecting = Connecting { _sourcePort         :: PortRef
                             , _destinationPortMay :: Maybe PortRef
                             , _history            :: DragHistory
                             } deriving (Eq, Show)

data State = State { _connecting  :: Maybe Connecting
                   } deriving (Eq, Show)


makeLenses ''State
makeLenses ''Connecting
makeLenses ''DragHistory


instance Default State where
    def = State def

instance PrettyPrinter State where
    display (State dragging) = "dS(" <> display dragging <> ")"

instance PrettyPrinter Connecting where
    display (Connecting source destinationMay history)
        = display source
        <> "->" <> display destinationMay
        <> "|"  <> display history

instance PrettyPrinter DragHistory where
    display (DragHistory start curr) = display start <> " " <> display curr
