module Empire.API.Data.NodeMeta where

import Prologue
import Data.Binary          (Binary)

import Empire.API.Data.Port (OutPort, InPort)

data NodeMeta = NodeMeta { _position :: (Double, Double)
                         } deriving (Generic, Show, Eq)

makeLenses ''NodeMeta

instance Binary NodeMeta
