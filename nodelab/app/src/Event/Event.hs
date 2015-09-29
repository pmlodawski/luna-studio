module Event.Event where

import           Utils.PreludePlus

import           Data.Dynamic
import           Object.Object ( Object )
import           Object.Dynamic

import qualified Event.Keyboard      as Keyboard
import qualified Event.Mouse         as Mouse
import qualified Event.Window        as Window
import qualified Event.NodeSearcher  as NodeSearcher
import qualified Event.WithObjects   as WithObjects
import qualified Event.Connection    as Connection
import qualified Event.ConnectionPen as ConnectionPen
import qualified Event.Batch         as Batch
import qualified Event.TextEditor    as TextEditor


data Event obj = Init
               | Window               Window.Event
               | Keyboard           Keyboard.Event
               | Mouse                 Mouse.Event
               | NodeSearcher   NodeSearcher.Event
               | Connection       Connection.Event
               | ConnectionPen ConnectionPen.Event
               | Batch                 Batch.Event
               | TextEditor       TextEditor.Event


makeLenses ''Event

instance Default (Event obj) where
    def = Init

instance Typeable obj => UnpackDynamic (Event Dynamic) (Event obj) where
    unpackDynamic Init               = Init
    unpackDynamic (Window        ev) = Window ev
    unpackDynamic (Keyboard      ev) = Keyboard ev
    unpackDynamic (Mouse         ev) = Mouse ev
    unpackDynamic (NodeSearcher  ev) = NodeSearcher ev
    unpackDynamic (Connection    ev) = Connection ev
    unpackDynamic (ConnectionPen ev) = ConnectionPen ev
    unpackDynamic (Batch         ev) = Batch ev
    unpackDynamic (TextEditor    ev) = TextEditor ev

instance PrettyPrinter obj => PrettyPrinter (Event obj) where
    display Init                = "InitEv"
    display (Window         ev) = "WinEv(" <> display ev <> ")"
    display (Keyboard       ev) = "KeyEv(" <> display ev <> ")"
    display (Mouse          ev) = "MouEv(" <> display ev <> ")"
    display (NodeSearcher   ev) = "NoSEv(" <> display ev <> ")"
    display (Connection     ev) = "ConEv(" <> display ev <> ")"
    display (ConnectionPen  ev) = "ConPn(" <> display ev <> ")"
    display (Batch          ev) = "BchEv(" <> display ev <> ")"
    display (TextEditor     ev) = "TxtEd(" <> display ev <> ")"
