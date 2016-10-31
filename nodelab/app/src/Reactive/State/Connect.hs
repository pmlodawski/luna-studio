module Reactive.State.Connect where


import           Data.Aeson               (ToJSON)
import           Utils.PreludePlus
import           Utils.Vector

import           Empire.API.Data.PortRef  (AnyPortRef)
import           Empire.API.JSONInstances ()


data Connecting = ConnectingFromPort FromPort
                | ConnectingFromEdge FromEdge
                deriving (Eq, Show, Generic)

data FromPort = FromPort
              { _sourcePortRef      :: AnyPortRef
              , _sourcePortAngleVec :: Vector2 Double
              , _sourceNodePos      :: Vector2 Double
              } deriving (Eq, Show, Generic)

data FromEdge = FromEdge
              deriving (Eq, Show, Generic)

data State = State { _connecting  :: Maybe Connecting
                   } deriving (Eq, Show, Generic)


makeLenses ''State
makeLenses ''FromPort
makeLenses ''FromEdge


instance ToJSON State
instance ToJSON Connecting
instance ToJSON FromEdge
instance ToJSON FromPort

instance Default State where
    def = State def

connectingFromPort :: AnyPortRef -> Vector2 Double -> Vector2 Double -> Connecting
connectingFromPort = ConnectingFromPort .:. FromPort

connectingFromEdge :: Connecting
connectingFromEdge = ConnectingFromEdge FromEdge
