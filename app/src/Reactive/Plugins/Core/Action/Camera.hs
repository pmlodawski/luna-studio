module Reactive.Plugins.Core.Action.Camera where

import           Prelude       hiding       ( mapM_, forM_ )
import           Data.Foldable              ( mapM_, forM_ )
import           Control.Lens
import           Control.Applicative
import           Data.Default
import           Data.Maybe
import           Data.List
import           Data.Char
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
            | MoveRight
            | MoveUp
            | MoveDown
            deriving (Eq, Show)


makeLenses ''Action

instance PrettyPrinter Action where
    display ZoomIn    = "cA( ZoomIn )"
    display ZoomOut   = "cA( ZoomOut )"
    display MoveLeft  = "cA( MoveLeft )"
    display MoveRight = "cA( MoveRight )"
    display MoveUp    = "cA( MoveUp )"
    display MoveDown  = "cA( MoveDown )"


toAction :: Event Node -> Maybe Action
toAction (Mouse (WithObjects mouseEvent objects)) = Nothing
toAction (Keyboard (Keyboard.Event Keyboard.Press char)) = case char of
    '='   -> Just ZoomIn
    '+'   -> Just ZoomIn
    '-'   -> Just ZoomOut
    _     -> Nothing
toAction (Keyboard (Keyboard.Event Keyboard.Down char)) = case char of
    '\0037' -> Just MoveLeft
    '\0039' -> Just MoveRight
    '\0038' -> Just MoveUp
    '\0040' -> Just MoveDown
    _     -> Nothing
toAction _ = Nothing


instance ActionStateUpdater Action where
    execSt newAction oldState = ActionUI newAction oldState



instance ActionUIUpdater Action where
    updatUI (WithState action state) = case action of
        ZoomIn      -> putStrLn $ display action
        ZoomOut     -> putStrLn $ display action
        MoveLeft    -> putStrLn $ display action
        MoveRight   -> putStrLn $ display action
        MoveUp      -> putStrLn $ display action
        MoveDown    -> putStrLn $ display action
