module Reactive.State.Connect where


import Utils.PreludePlus
import Utils.Vector

import Object.Node
import Object.UITypes
import Object.Port

data DragHistory = DragHistory { _dragStartPos    :: Vector2 Int
                               , _dragCurrentPos  :: Vector2 Int
                               } deriving (Eq, Show)

data Connecting = Connecting { _sourcePortRef      :: PortRef
                             , _sourcePortWidget   :: WidgetId
                             , _sourcePortAngleVec :: Vector2 Double
                             , _sourceNodePos      :: Vector2 Double
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
    display (Connecting sourceRef source _ _ destinationMay history)
        = "conn(" <> display sourceRef
        <> ":"    <> display source
        <> "->"   <> display destinationMay
        <> "|"    <> display history

instance PrettyPrinter DragHistory where
    display (DragHistory start curr) = display start <> " " <> display curr
