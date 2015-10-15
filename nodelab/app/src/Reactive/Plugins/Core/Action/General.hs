module Reactive.Plugins.Core.Action.General where

import           Utils.PreludePlus
import           Utils.Vector

import           JS.Bindings
import           Object.Object
import           Object.Node
import           Event.Mouse    hiding      ( Event, WithObjects )
import qualified Event.Mouse    as Mouse
import qualified Event.Window   as Window
import           Event.Event
import           Event.WithObjects
import           Reactive.Plugins.Core.Action
import qualified Reactive.Plugins.Core.Action.Camera         as Camera
import qualified Reactive.Plugins.Core.Action.State.Camera   as Camera
import qualified Reactive.Plugins.Core.Action.State.Global   as Global



data Action = Moving   { _pos   :: Vector2 Int }
            | Resizing { _size  :: Vector2 Int }
            deriving (Eq, Show)


makeLenses ''Action

instance PrettyPrinter Action where
    display (Moving point)  = "mA(" <> display point <> ")"
    display (Resizing size) = "rA(" <> display size  <> ")"


toAction :: Event Node -> Maybe Action
toAction (Mouse (Mouse.Event tpe pos _ _ _)) = case tpe of
    Mouse.Moved     -> Just $ Moving pos
    _               -> Nothing
toAction (Window (Window.Event tpe width height)) = case tpe of
    Window.Resized  -> Just $ Resizing $ Vector2 width height
toAction _           = Nothing


instance ActionStateUpdater Action where
    execSt newAction oldState = ActionUI newAction newState where
        newState           = case newAction of
            Moving pos    -> oldState & Global.iteration  +~ 1
                                      & Global.mousePos   .~ pos
            Resizing size -> oldState & Global.iteration  +~ 1
                                      & Global.camera . Camera.camera . Camera.screenSize .~ size


instance ActionUIUpdater Action where
    updateUI (WithState action state) = case action of
        Moving pos      -> updateMouse x y where
            Vector2 x y  = Camera.screenToWorkspace camera pos
            camera       = state ^. Global.camera . Camera.camera
        Resizing size   -> Camera.syncCamera state
                        >> updateScreenSize (size ^. x) (size ^. y)
