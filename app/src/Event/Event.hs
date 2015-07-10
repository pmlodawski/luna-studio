module Event.Event where

import Data.Monoid
import Data.Typeable
import Data.Dynamic
import Data.Default
import Control.Lens

import Object.Object ( Object )
import Object.Dynamic
import Utils.PrettyPrinter

import qualified Event.Keyboard      as Keyboard
import qualified Event.Mouse         as Mouse
import qualified Event.Window        as Window
import qualified Event.NodeSearcher  as NodeSearcher
import qualified Event.WithObjects   as WithObjects


data Event obj = Keyboard     Keyboard.Event
               | Mouse        (Mouse.MEvent obj)
               | Window       Window.Event
               | NodeSearcher NodeSearcher.Event


makeLenses ''Event

instance Typeable obj => UnpackDynamic (Event Dynamic) (Event obj) where
    unpackDynamic (Mouse (WithObjects.WithObjects ev obj)) = Mouse (WithObjects.WithObjects ev $ unpackDynamic obj)
    unpackDynamic (Window ev)                              = Window ev
    unpackDynamic (Keyboard ev)                            = Keyboard ev
    unpackDynamic (NodeSearcher ev)                        = NodeSearcher ev

instance PrettyPrinter obj => PrettyPrinter (Event obj) where
    display (Keyboard ev)                          = "KeyEv(" <> display ev <> ")"
    display (Window ev)                            = "WinEv(" <> display ev <> ")"
    display (NodeSearcher ev)                      = "NoSEv(" <> display ev <> ")"
    display (Mouse (WithObjects.WithObjects ev o)) = "MouEv(" <> display ev <> ")"
