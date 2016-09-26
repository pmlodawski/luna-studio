module Event.Event where

import           Utils.PreludePlus
import           GHCJS.Types (JSVal)
import           GHCJS.Marshal.Pure (PToJSVal(..), PFromJSVal(..))
import           Data.Aeson (ToJSON, toJSON)

import qualified Event.Keyboard      as Keyboard
import qualified Event.Mouse         as Mouse
import qualified Event.Window        as Window
import qualified Event.NodeSearcher  as NodeSearcher
import qualified Event.Connection    as Connection
import qualified Event.ConnectionPen as ConnectionPen
import qualified Event.Batch         as Batch
import qualified Event.TextEditor    as TextEditor
import qualified Event.Debug         as Debug
import qualified Event.CustomEvent   as CustomEvent
import qualified Event.Widget        as Widget

newtype JSState = JSState JSVal deriving (PFromJSVal, PToJSVal)

instance Eq JSState where
    _ == _ = True

instance Show JSState where
    show _ = "JSState"

data Event = Init
           | Window                       Window.Event
           | Keyboard      JSState      Keyboard.Event
           | Mouse         JSState      Mouse.RawEvent
           | NodeSearcher           NodeSearcher.Event
           | Connection               Connection.Event
           | ConnectionPen         ConnectionPen.Event
           | Batch                         Batch.Event
           | TextEditor               TextEditor.Event
           | Debug                         Debug.Event
           | CustomEvent             CustomEvent.Event
           | Widget                       Widget.Event
           | Tick
           deriving (Generic, Show)

makeLenses ''Event

instance Default Event where
    def = Init

instance ToJSON Event

instance ToJSON JSState where
    toJSON _ = toJSON "(..)"

name :: Getter Event String
name = to $ \n -> case n of
    Init              -> "Init"
    Window        _   -> "Window"
    Keyboard      _ _ -> "Keyboard"
    Mouse         _ _ -> "Mouse"
    NodeSearcher  _   -> "NodeSearcher"
    Connection    _   -> "Connection"
    ConnectionPen _   -> "ConnectionPen"
    Batch         _   -> "Batch"
    TextEditor    _   -> "TextEditor"
    Debug         _   -> "Debug"
    CustomEvent   _   -> "CustomEvent"
    Widget        _   -> "Widget"
    Tick              -> "Tick"
