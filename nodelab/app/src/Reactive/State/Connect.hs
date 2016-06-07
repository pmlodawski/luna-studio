module Reactive.State.Connect where


import Utils.PreludePlus
import Utils.Vector
import Data.Aeson (ToJSON)

import Object.UITypes
import Empire.API.Data.PortRef (OutPortRef, InPortRef, AnyPortRef)
import Empire.API.JSONInstances ()

data DragHistory = DragHistory { _dragStartPos    :: Vector2 Int
                               , _dragCurrentPos  :: Vector2 Int
                               } deriving (Eq, Show, Generic)

data Connecting = Connecting { _sourcePortRef      :: AnyPortRef
                             , _sourcePortAngleVec :: Vector2 Double
                             , _sourceNodePos      :: Vector2 Double
                             , _destinationPortMay :: Maybe AnyPortRef
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
