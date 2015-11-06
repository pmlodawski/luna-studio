module Reactive.State.Connect where


import Utils.PreludePlus
import Utils.Vector

import Object.Node
import Object.UITypes
import Object.Port
import Data.Aeson (ToJSON)

data DragHistory = DragHistory { _dragStartPos    :: Vector2 Int
                               , _dragCurrentPos  :: Vector2 Int
                               } deriving (Eq, Show, Generic)

data Connecting = Connecting { _sourcePortRef      :: PortRef
                             , _sourcePortWidget   :: WidgetId
                             , _sourcePortAngleVec :: Vector2 Double
                             , _sourceNodePos      :: Vector2 Double
                             , _destinationPortMay :: Maybe PortRef
                             , _history            :: DragHistory
                             } deriving (Eq, Show, Generic)

data State = State { _connecting  :: Maybe Connecting
                   } deriving (Eq, Show, Generic)


makeLenses ''State
makeLenses ''Connecting
makeLenses ''DragHistory

instance ToJSON State
instance ToJSON Connecting
instance ToJSON DragHistory

instance Default State where
    def = State def
