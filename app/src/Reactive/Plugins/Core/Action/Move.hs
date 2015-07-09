module Reactive.Plugins.Core.Action.Move where

import           Prelude       hiding       ( mapM_, forM_ )
import           Data.Foldable              ( mapM_, forM_ )
import           Control.Lens
import           Data.Default
import           Data.Maybe
import           Data.List
import           Data.Monoid
import           Data.Function
import           System.Mem

import           JS.Bindings
import           JS.Appjs
import           Object.Object
import qualified Object.Node    as Node     ( position )
import           Object.Node    hiding      ( position )
import           Event.Keyboard hiding      ( Event )
import qualified Event.Keyboard as Keyboard
import           Event.Mouse    hiding      ( Event, WithObjects )
import qualified Event.Mouse    as Mouse
import           Event.Event
import           Event.WithObjects
import           Utils.Wrapper
import           Utils.PrettyPrinter
import           Reactive.Plugins.Core.Action.Action
import qualified Reactive.Plugins.Core.Action.State.Global   as Global



data Action = Moving { _pos :: Point }
            deriving (Eq, Show)


makeLenses ''Action

instance PrettyPrinter Action where
    display (Moving point) = "mA( " <> display point <> " )"


toAction :: Event Node -> Maybe Action
toAction (Mouse (WithObjects (Mouse.Event tpe pos _ _) _)) = case tpe of
    Mouse.Moved -> Just $ Moving pos
    _           -> Nothing
toAction _       = Nothing


instance ActionStateUpdater Action where
    execSt newAction oldState = ActionUI newAction newState
        where
        newPos                           = newAction ^. pos
        newState                         = oldState & Global.iteration +~ 1
                                                    & Global.mousePos .~ newPos


instance ActionUIUpdater Action where
    updateUI (WithState action state) = return ()
