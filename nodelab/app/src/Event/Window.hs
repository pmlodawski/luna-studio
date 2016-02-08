module Event.Window where

import           Data.Aeson        (ToJSON)
import           Utils.PreludePlus


data Type = Resized deriving (Eq, Show, Generic)


data Event = Event { _tpe         :: Type
                   , _innerWidth  :: Int
                   , _innerHeight :: Int
                   } deriving (Eq, Show, Generic)

makeLenses ''Event

instance PrettyPrinter Event where
    display (Event t w h) = show t <> " " <> show w <> " " <> show h


instance ToJSON Event
instance ToJSON Type
