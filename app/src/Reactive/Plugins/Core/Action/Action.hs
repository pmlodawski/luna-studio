{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Reactive.Plugins.Core.Action.Action where

import           Data.Monoid          ( (<>) )
import           Data.Default
-- import
import           Data.Maybe           ( isJust )
import           Data.Functor
import           Control.Lens

import           Reactive.Plugins.Core.Action.State.Global
import           Utils.PrettyPrinter

data WithState act st = WithState { _action :: act
                                  , _state  :: st
                                  } -- deriving (Show)

type WithStateMaybe act st = WithState (Maybe act) st

makeLenses ''WithState



instance (Default act, Default st) => Default (WithState act st) where
    def = WithState def def

instance (PrettyPrinter act, PrettyPrinter st) => PrettyPrinter (WithState act st) where
    display (WithState action state) = "na( " <> display action <> " " <> display state <> " )"


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
    updatUI :: WithState act State -> IO ()

instance ActionUIUpdater act => ActionUIUpdater (Maybe act) where
    updatUI (WithState (Just action) state) = updatUI (WithState action state)
    updatUI (WithState  Nothing      _    ) = return ()

data NoAction = NoAction

instance PrettyPrinter NoAction where
    display a = "NoAction"

instance ActionUIUpdater NoAction where
    updatUI (WithState NoAction state) = return ()


noActionUI :: State -> ActionUI
noActionUI st = ActionUI NoAction st

updatAllUI :: [ActionUI] -> IO ()
updatAllUI [] =  return ()
updatAllUI ((ActionUI act st):as) = updatUI (WithState act st) >> updatAllUI as

logAllUI :: [ActionUI] -> IO ()
logAllUI [] = putStrLn "-"
logAllUI ((ActionUI act st):as) = do
    putStrLn $ (display st) <> " <- " <> (display act)
    logAllUI as

getState :: ActionUI -> State
getState (ActionUI _ st) = st
