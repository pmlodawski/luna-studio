{-# LANGUAGE DeriveAnyClass #-}

module NodeEditor.Event.View where

import           Common.Analytics            (IsTrackedEvent (isTracked))
import           Common.Data.Event           (EventName (eventName), consName)
import           Common.Prelude
import           Control.Lens.Aeson          (parseDropUnary, toEncodingDropUnary)
import           Data.Aeson                  (FromJSON (..), ToJSON (..))


type Path = [String]

data ViewEvent = ViewEvent
    { _path   :: Path
    , _target :: Maybe String
    , _base   :: BaseEvent
    } deriving (Generic, Show, NFData)

data BaseEvent
    = MouseEvent
        { altKey       :: Bool
        , bubbles      :: Bool
        , button       :: Int
        , buttons      :: Double
        , cancelable   :: Bool
        , cancelBubble :: Bool
        , clientX      :: Double
        , clientY      :: Double
        , ctrlKey      :: Bool
        , defaultPrevented :: Bool
        , detail      :: Double
        , eventPhase  :: Double
        , isTrusted   :: Bool
        , layerX      :: Double
        , layerY      :: Double
        , metaKey     :: Bool
        , movementY   :: Double
        , pageX       :: Double
        , pageY       :: Double
        , screenX     :: Double
        , shiftKey    :: Bool
        , screenY     :: Double
        , movementX   :: Double
        , timeStamp   :: Double
        , offsetX     :: Double
        , offsetY     :: Double
        , returnValue :: Bool
        , type_       :: String
        , which       :: Double
        , x           :: Double
        , y           :: Double
        }
    deriving (Generic, Show, NFData)

makeLenses ''ViewEvent

instance ToJSON ViewEvent where toEncoding = toEncodingDropUnary
instance ToJSON BaseEvent where toEncoding = toEncodingDropUnary

instance FromJSON ViewEvent where parseJSON = parseDropUnary
instance FromJSON BaseEvent where parseJSON = parseDropUnary

instance EventName ViewEvent where
    eventName = intercalate "." . view path
instance IsTrackedEvent ViewEvent where
    isTracked = const False

mouseAltKey :: BaseEvent -> Bool
mouseAltKey = \case
    MouseEvent { altKey = key } -> key
    _ -> False

mouseCtrlKey :: BaseEvent -> Bool
mouseCtrlKey = \case
    MouseEvent { ctrlKey = key } -> key
    _ -> False

mouseShiftKey :: BaseEvent -> Bool
mouseShiftKey = \case
    MouseEvent { shiftKey = key } -> key
    _ -> False

mouseMetaKey :: BaseEvent -> Bool
mouseMetaKey = \case
    MouseEvent { metaKey = key } -> key
    _ -> False

mouseButton ::  Int -> BaseEvent -> Bool
mouseButton btn = \case
    MouseEvent { button = btn' } -> btn == btn'
    _ -> False

leftButton :: Int
leftButton = 0

middleButton :: Int
middleButton = 1

rightButton :: Int
rightButton = 2

withoutMods :: BaseEvent -> Int -> Bool
withoutMods evt b = mouseButton b evt
    && not (mouseAltKey   evt)
    && not (mouseCtrlKey  evt)
    && not (mouseShiftKey evt)
    && not (mouseMetaKey  evt)

withCtrl :: BaseEvent -> Int -> Bool
withCtrl evt b = mouseButton b evt
    && (mouseCtrlKey evt || mouseMetaKey evt)

withAlt :: BaseEvent -> Int -> Bool
withAlt evt b = mouseButton b evt
    &&      mouseAltKey   evt
    && not (mouseCtrlKey  evt)
    && not (mouseShiftKey evt)
    && not (mouseMetaKey  evt)

withShift :: BaseEvent -> Int -> Bool
withShift evt b = mouseButton b evt
    && not (mouseAltKey   evt)
    && not (mouseCtrlKey  evt)
    &&      mouseShiftKey evt
    && not (mouseMetaKey  evt)

withCtrlAlt :: BaseEvent -> Int -> Bool
withCtrlAlt evt b = mouseButton b evt
    &&      mouseAltKey   evt
    && not (mouseShiftKey evt)
    && (mouseCtrlKey evt || not (mouseMetaKey  evt))

withCtrlShift :: BaseEvent -> Int -> Bool
withCtrlShift evt b = mouseButton b evt
    && not (mouseAltKey   evt)
    &&      mouseShiftKey evt
    && (mouseCtrlKey evt || not (mouseMetaKey  evt))

withAltShift :: BaseEvent -> Int -> Bool
withAltShift evt b = mouseButton b evt
    &&      mouseAltKey   evt
    && not (mouseCtrlKey  evt)
    &&      mouseShiftKey evt
    && not (mouseMetaKey  evt)

withCtrlAltShift :: BaseEvent -> Int -> Bool
withCtrlAltShift evt b = mouseButton b evt
    && mouseAltKey   evt
    && mouseShiftKey evt
    && (mouseCtrlKey evt || not (mouseMetaKey evt))
