module Event.Mouse where


import Utils.PreludePlus

import Event.Keyboard  (KeyMods(..))
import Utils.Vector
import Object.UITypes
import Data.Aeson (ToJSON)

data MouseButton = NoButton
                 | LeftButton
                 | MiddleButton
                 | RightButton
                 deriving (Eq, Show, Generic)

instance ToJSON MouseButton

toMouseButton :: Int -> MouseButton
toMouseButton 1  = LeftButton
toMouseButton 2  = MiddleButton
toMouseButton 3  = RightButton
toMouseButton _  = NoButton

type MousePosition = Vector2 Int

data Type = Pressed
          | Released
          | Moved
          | Clicked
          | DblClicked
          | Wheel (Vector2 Double)
          deriving (Eq, Show)

data EventWidget = EventWidget { _widgetId    :: WidgetId
                               , _worldMatrix :: [Double]
                               , _scene       :: SceneType
                               } deriving (Eq, Show)

makeLenses ''EventWidget

data Event a = Event { _tpe         :: Type
                     , _position    :: Vector2 a
                     , _button      :: MouseButton
                     , _keyMods     :: KeyMods
                     , _widget      :: Maybe EventWidget
                     } deriving (Eq, Show, Typeable)

type RawEvent  = Event Int
type Event'    = Event Double

makeLenses ''Event

instance PrettyPrinter MouseButton where
    display = show

instance PrettyPrinter EventWidget where
    display = show

instance PrettyPrinter Type where
    display = show

instance (PrettyPrinter a) => PrettyPrinter (Event a) where
    display (Event tpe pos button keyMods widget) = "ev(" <> display tpe     <>
                                                    " "   <> display pos     <>
                                                    " "   <> display button  <>
                                                    " "   <> display keyMods <>
                                                    " "   <> display widget  <>
                                                    ")"
