{-# LANGUAGE DeriveAnyClass #-}

module NodeEditor.Event.View where

import           Common.Analytics            (IsTrackedEvent (isTracked))
import           Common.Data.Event           (EventName (eventName), consName)
import           Common.Prelude
import           Control.Lens.Aeson          (parseDropUnary, toEncodingDropUnary)
import           Data.Aeson                  (FromJSON (..), ToJSON (..))


type Path = [String]

data ViewEvent = ViewEvent
    { _path :: Path
    , _base :: BaseEvent
    } deriving (Generic, Show, NFData)

data BaseEvent
    = MouseEvent
        { _altKey       :: Bool
        , _bubbles      :: Bool
        , _button       :: Int
        , _buttons      :: Double
        , _cancelable   :: Bool
        , _cancelBubble :: Bool
        , _clientX      :: Double
        , _clientY      :: Double
        , _ctrlKey      :: Bool
        , _defaultPrevented :: Bool
        , _detail      :: Double
        , _eventPhase  :: Double
        , _isTrusted   :: Bool
        , _layerY      :: Double
        , _metaKey     :: Bool
        , _movementY   :: Double
        , _pageX       :: Double
        , _screenX     :: Double
        , _shiftKey    :: Bool
        , _screenY     :: Double
        , _layerX      :: Double
        , _movementX   :: Double
        , _timeStamp   :: Double
        , _offsetY     :: Double
        , _offsetX     :: Double
        , _pageY       :: Double
        , _returnValue :: Bool
        , _type_       :: String
        , _which       :: Double
        , _x           :: Double
        , _y           :: Double
        }
    | Test
    deriving (Generic, Show, NFData)

makeLenses ''ViewEvent
makeLenses ''BaseEvent

instance ToJSON ViewEvent where toEncoding = toEncodingDropUnary
instance ToJSON BaseEvent where toEncoding = toEncodingDropUnary

instance FromJSON ViewEvent where parseJSON = parseDropUnary
instance FromJSON BaseEvent where parseJSON = parseDropUnary

instance EventName ViewEvent where
    eventName = intercalate "." . view path
instance IsTrackedEvent ViewEvent where
    isTracked = const False
