module Event.NodeSearcher where


import           Data.Monoid
import           Control.Lens

import           Utils.PrettyPrinter

import qualified Data.Text.Lazy as Text
import           Data.Text.Lazy (Text)


data Event = Event { _action     :: Text
                   , _expression :: Text
                   } deriving (Eq, Show)

makeLenses ''Event

instance PrettyPrinter Event where
    display (Event action expr) = "NSev(" <> show action <> " -> " <> show expr <> ")"

