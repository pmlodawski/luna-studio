module Reactive.Plugins.Core.Action.General where

import           Control.Lens
import           Data.Maybe
import           Data.Monoid

import           Object.Object
import           Object.Node    hiding      ( position )
import           Event.Mouse    hiding      ( Event, WithObjects )
import qualified Event.Mouse    as Mouse
import qualified Event.Window   as Window
import           Event.Event
import           Event.WithObjects
import           Utils.PrettyPrinter
import           Reactive.Plugins.Core.Action.Action
import qualified Reactive.Plugins.Core.Action.State.Global   as Global



data Action = Moving   { _pos   :: Point }
            | Resizing { _size  :: Point }
            deriving (Eq, Show)


makeLenses ''Action

instance PrettyPrinter Action where
    display (Moving point)  = "mA( " <> display point <> " )"
    display (Resizing size) = "rA( " <> display size  <> " )"


toAction :: Event Node -> Maybe Action
toAction (Mouse (WithObjects (Mouse.Event tpe pos _ _) _)) = case tpe of
    Mouse.Moved     -> Just $ Moving pos
    _               -> Nothing
toAction (Window (Window.Event tpe width height)) = case tpe of
    Window.Resized  -> Just $ Resizing $ Point width height
toAction _           = Nothing


instance ActionStateUpdater Action where
    execSt newAction oldState = ActionUI newAction newState where
        newState           = case newAction of
            Moving pos    -> oldState & Global.iteration  +~ 1
                                      & Global.mousePos   .~ pos
            Resizing size -> oldState & Global.iteration  +~ 1
                                      & Global.screenSize .~ size


instance ActionUIUpdater Action where
    updateUI (WithState action state) = return ()
