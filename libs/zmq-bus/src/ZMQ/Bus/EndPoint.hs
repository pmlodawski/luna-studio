module ZMQ.Bus.EndPoint where

import           Flowbox.Config.Config (Config)
import qualified Flowbox.Config.Config as Config
import           Flowbox.Prelude



type EndPoint = String


data BusEndPoints = BusEndPoints { controlEndPoint :: EndPoint
                                 , pullEndPoint    :: EndPoint
                                 , pubEndPoint     :: EndPoint
                                 } deriving (Read, Show, Eq)


clientFromConfig :: Config -> BusEndPoints
clientFromConfig config = BusEndPoints ctrl pull pub where
    ctrl = Config.clientControlEndPoint bus
    pull = Config.clientPullEndPoint    bus
    pub  = Config.clientPubEndPoint     bus
    bus  = Config.bus config


serverFromConfig :: Config -> BusEndPoints
serverFromConfig config = BusEndPoints ctrl pull pub where
    ctrl = Config.serverControlEndPoint bus
    pull = Config.serverPullEndPoint    bus
    pub  = Config.serverPubEndPoint     bus
    bus  = Config.bus config
