module Reactive.Plugins.Core.Action.Camera where

import           Prelude       hiding       ( mapM_, forM_ )
import           Data.Foldable              ( mapM_, forM_ )
import           Control.Lens
import           Control.Applicative
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
import           Event.Mouse    hiding      ( Event )
import qualified Event.Mouse    as Mouse
import           Event.Event
import           Event.WithObjects
import           Utils.Wrapper
import           Utils.PrettyPrinter
import           Reactive.Plugins.Core.Action.Action
import qualified Reactive.Plugins.Core.Action.State.Global    as Global


data Action = ZoomIn
            | ZoomOut
            | MoveLeft
            deriving (Eq, Show)


makeLenses ''Action

instance PrettyPrinter Action where
    display ZoomIn    = "cA( ZoomIn )"
    display ZoomOut   = "cA( ZoomOut )"
    display MoveLeft  = "cA( MoveLeft )"


toAction :: Event Node -> Maybe Action
toAction (Mouse (WithObjects mouseEvent objects)) = Nothing
toAction (Keyboard (Keyboard.Event char)) = case char of
    '=' -> Just ZoomIn
    '+' -> Just ZoomIn
    '-' -> Just ZoomOut
    _   -> Nothing


instance ActionStateExecutor Action Global.State where
    exec newAction oldState = WithState (Just newAction) oldState


updateUI :: WithStateMaybe Action Global.State -> IO ()
updateUI (WithState maybeAction state) = case maybeAction of
    Nothing         -> return ()
    Just action     -> case action of
        ZoomIn      -> putStrLn $ display action
        ZoomOut     -> putStrLn $ display action
        MoveLeft    -> putStrLn $ display action
