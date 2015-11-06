module Reactive.State.MultiSelection where


import Utils.PreludePlus
import Utils.Vector

import Object.Object

data DragHistory = DragHistory { _dragStartPos    :: Vector2 Int
                               , _dragCurrentPos  :: Vector2 Int
                               } deriving (Eq, Show)


data State = State { _history  :: Maybe DragHistory
                   } deriving (Eq, Show)


makeLenses ''State
makeLenses ''DragHistory


instance Default State where
    def = State def

instance PrettyPrinter State where
    display (State dragging) = "dS(" <> display dragging <> ")"

instance PrettyPrinter DragHistory where
    display (DragHistory start curr) = display start <> " " <> display curr
