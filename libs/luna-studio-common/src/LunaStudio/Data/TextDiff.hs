module LunaStudio.Data.TextDiff where

import Control.Lens          (each)
import Data.Aeson.Types      (FromJSON, ToJSON)
import Data.Binary           (Binary)
import LunaStudio.Data.Point (Point)
import Prologue


data TextDiff = TextDiff
    { _range   :: Maybe (Point, Point)
    , _newText :: Text
    , _cursor  :: Maybe Point
    } deriving (Eq, Generic, Show)

makeLenses ''TextDiff
instance Binary   TextDiff
instance NFData   TextDiff
instance FromJSON TextDiff
instance ToJSON   TextDiff

mkTextDiff :: (Int,Int) -> (Int, Int) -> Text -> Maybe (Int, Int) -> TextDiff
mkTextDiff start end t c = TextDiff
    (Just (convert start, convert end))
    t
    (convert <$> c)
