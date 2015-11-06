module Object.Widget.Connection where

import Utils.PreludePlus hiding (from)
import Utils.Vector
import Object.Object
import Object.UITypes
import Object.Widget
import Data.Aeson (ToJSON)

data Connection = Connection { _connectionId :: ConnectionId
                             , _visible      :: Bool
                             , _from         :: Vector2 Double
                             , _to           :: Vector2 Double
                             , _color        :: Int
                             } deriving (Eq, Show, Typeable, Generic)

makeLenses ''Connection
instance ToJSON Connection

instance IsDisplayObject Connection where
    widgetPosition = from

data CurrentConnection = CurrentConnection { _currentVisible      :: Bool
                                           , _currentFrom         :: Vector2 Double
                                           , _currentTo           :: Vector2 Double
                                           , _currentColor        :: Int
                                           } deriving (Eq, Show, Typeable, Generic)

makeLenses ''CurrentConnection
instance ToJSON CurrentConnection

instance IsDisplayObject CurrentConnection where
    widgetPosition = currentFrom
