{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Reactive.Plugins.Core.Action where

import           Utils.PreludePlus
import           JS.Bindings

import           Reactive.State.Global
import qualified Reactive.State.UIRegistry as UIRegistry
import           Reactive.Commands.Command (Command, execCommand)

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

instance ActionStateUpdater act => ActionStateUpdater [act] where
    execSt [] state = ActionUI NoAction state
    execSt (first : rest) state = case execSt first state of
        ActionUI firstAct newState -> case execSt rest newState of
            ActionUI lastAct finalState -> ActionUI (SequenceUI firstAct lastAct) finalState

instance ActionStateUpdater (Command State ()) where
    execSt cmd state = ActionUI (PerformIOAction action) newState where
        (action, newState) = execCommand cmd state




data ActionUI = forall act. (ActionUIUpdater act, PrettyPrinter act) => ActionUI act State

data SequenceUI = forall a1 a2. (ActionUIUpdater a1, PrettyPrinter a1,
                                 ActionUIUpdater a2, PrettyPrinter a2) => SequenceUI a1 a2

data PerformIOAction = PerformIOAction (IO ())

instance PrettyPrinter PerformIOAction where
    display _ = "PerformIOAction"

instance ActionUIUpdater PerformIOAction where
    updateUI (WithState (PerformIOAction act) _) = act

instance PrettyPrinter SequenceUI where
    display (SequenceUI a1 a2) = "seq(" <> display a1 <> ", " <> display a2 <> ")"

class ActionUIUpdater act where
    updateUI :: WithState act State -> IO ()

instance ActionUIUpdater SequenceUI where
    updateUI (WithState (SequenceUI a1 a2) state) = updateUI (WithState a1 state)
                                                 >> updateUI (WithState a2 state)

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
