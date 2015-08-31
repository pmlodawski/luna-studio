{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Reactive.Plugins.Core.Action.Action where

import           Utils.PreludePlus
import           JS.Bindings

import           Reactive.Plugins.Core.Action.State.Global
import qualified Reactive.Plugins.Core.Action.State.UIRegistry as UIRegistry

data WithState act st = WithState { _action :: act
                                  , _state  :: st
                                  } deriving (Eq, Show)

type WithStateMaybe act st = WithState (Maybe act) st

makeLenses ''WithState



instance (Default act, Default st) => Default (WithState act st) where
    def = WithState def def

instance (PrettyPrinter act, PrettyPrinter st) => PrettyPrinter (WithState act st) where
    display (WithState action state) = "na(" <> display action <> " " <> display state <> ")"


data ActionST = forall act. (ActionStateUpdater act, PrettyPrinter act) => ActionST act

class ActionStateUpdater act where
    execSt :: act -> State -> ActionUI

instance ActionStateUpdater act => ActionStateUpdater (Maybe act) where
    execSt (Just action) state = execSt action state
    execSt Nothing       state = ActionUI NoAction state

instance ActionStateUpdater ActionST where
    execSt (ActionST act) state = execSt act state




data ActionUI = forall act. (ActionUIUpdater act, PrettyPrinter act) => ActionUI act State

class ActionUIUpdater act where
    updateUI :: WithState act State -> IO ()

instance ActionUIUpdater act => ActionUIUpdater (Maybe act) where
    updateUI (WithState (Just action) state) = updateUI (WithState action state)
    updateUI (WithState  Nothing      _    ) = return ()

data NoAction = NoAction

instance PrettyPrinter NoAction where
    display a = "NoAction"

instance ActionUIUpdater NoAction where
    updateUI (WithState NoAction state) = return ()


noActionUI :: State -> ActionUI
noActionUI st = ActionUI NoAction st

updateAllUI :: [ActionUI] -> IO ()
updateAllUI [] = shouldRender
updateAllUI ((ActionUI act st):as) = updateUI (WithState act st) >> updateAllUI as

logAllUI :: [ActionUI] -> IO ()
logAllUI [] = return ()
logAllUI ((ActionUI act st):as) = let actString = display act in case actString of
    "NoAction" -> logAllUI as
    _          -> do
                    putStrLn $ (display st) <> " <- " <> actString
                    logAllUI as

getState :: ActionUI -> State
getState (ActionUI _ st) = st

ifNoneFocused :: State -> Maybe a -> Maybe a
ifNoneFocused st a = case st ^. uiRegistry . UIRegistry.focusedWidget of
    Just _  -> Nothing
    Nothing -> a
