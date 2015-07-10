module Event.Window where

import Data.Monoid
import Control.Lens
import Utils.PrettyPrinter

data Type = Resize deriving (Eq, Show)


data Event = Event { _tpe         :: Type
                   , _innerWidth  :: Int
                   , _innerHeight :: Int
                   } deriving (Eq, Show)

makeLenses ''Event

instance PrettyPrinter Event where
    display (Event t w h) = show t <> " " <> show w <> " " <> show h

