module Empire.API.Data.NodeMeta where

import Prologue
import Data.Aeson           (FromJSON, ToJSON)
import Data.Binary          (Binary)

data NodeMeta = NodeMeta { _position      :: (Double, Double)
                         , _displayResult :: Bool
                         } deriving (Generic, Show, Eq, Ord)

makeLenses ''NodeMeta

instance Default NodeMeta where
    def = NodeMeta def True

instance Binary NodeMeta

instance ToJSON NodeMeta
instance FromJSON NodeMeta
