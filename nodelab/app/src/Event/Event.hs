module Event.Event where

import           Utils.PreludePlus
import           GHCJS.Types (JSVal)
import           GHCJS.Marshal.Pure (PToJSVal(..), PFromJSVal(..))

import qualified Event.Keyboard      as Keyboard
import qualified Event.Mouse         as Mouse
import qualified Event.Window        as Window
import qualified Event.NodeSearcher  as NodeSearcher
import qualified Event.Connection    as Connection
import qualified Event.ConnectionPen as ConnectionPen
import qualified Event.Batch         as Batch
import qualified Event.TextEditor    as TextEditor
import qualified Event.Debug         as Debug

newtype JSState = JSState JSVal deriving (PFromJSVal, PToJSVal)

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

makeLenses ''Event

instance Default Event where
    def = Init
