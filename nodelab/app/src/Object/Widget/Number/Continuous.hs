module Object.Widget.Number.Continuous where

import Utils.PreludePlus
import Utils.Vector
import Data.Fixed
import Object.Widget
import Numeric
import Data.Aeson (ToJSON)

data ContinuousNumber =  ContinuousNumber { _position       :: Vector2 Double
                                          , _size           :: Vector2 Double
                                          , _label          :: Text
                                          , _value          :: Double
                                          , _enabled        :: Bool
                                          , _dragStartValue :: Maybe Double
                                          } deriving (Eq, Show, Typeable, Generic)

makeLenses ''ContinuousNumber
instance ToJSON ContinuousNumber

create :: Size -> Text -> Double -> ContinuousNumber
create s l v = ContinuousNumber def s l v True def

instance IsDisplayObject ContinuousNumber where
    widgetPosition = position
    widgetSize     = size

displayValue' :: ContinuousNumber -> String
displayValue' model = showGFloatAlt (Just 2) (model ^. value) ""

displayValue :: Getter ContinuousNumber String
displayValue = to displayValue'
