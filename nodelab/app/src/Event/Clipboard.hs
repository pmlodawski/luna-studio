module Event.Clipboard where

import           Data.Aeson        (ToJSON, toJSON)
import           GHCJS.Types
import           Utils.PreludePlus

data Event = Copy
           | Cut
           | Paste Text
             deriving (Eq, Show, Generic)

makeLenses ''Event

instance ToJSON Event
