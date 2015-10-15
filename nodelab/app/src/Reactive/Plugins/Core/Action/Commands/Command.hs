{-# LANGUAGE TypeFamilies #-}

module Reactive.Plugins.Core.Action.Commands.Command where

import Utils.PreludePlus
import Control.Monad.State
import Control.Monad.Writer
import Control.Lens.Zoom
import Control.Lens.Internal.Zoom


newtype IOAction = IOAction { unIOAction :: IO () }

instance Monoid IOAction where
    mempty = IOAction $ return ()
    mappend (IOAction a) (IOAction b) = IOAction $ a >> b

newtype Command a b = Command { unCommand :: StateT a (Writer IOAction) b }
                    deriving (Functor, Applicative, Monad, MonadWriter IOAction, MonadState a)

instance PrettyPrinter (Command a b) where
    display _ = "Command"

type instance Zoomed (Command a) = Focusing (Writer IOAction)

instance Zoom (Command s) (Command t) s t where
    zoom l (Command m) = Command (zoom l m)

command :: (a -> (IO (), a)) -> Command a ()
command f = do
    (action, state) <- gets f
    performIO action
    put state

pureCommand :: (a -> a) -> Command a ()
pureCommand = modify

ioCommand :: (a -> IO ()) -> Command a ()
ioCommand f = gets f >>= performIO

runCommand :: Command a b -> a -> (b, IO (), a)
runCommand cmd state = case runWriter (runStateT (unCommand cmd) state) of
    ((res, state), IOAction act) -> (res, act, state)

execCommand :: Command a b -> a -> (IO (), a)
execCommand cmd state = case runCommand cmd state of
    (_, act, state) -> (act, state)

performIO :: IO () -> Command a ()
performIO action = tell $ IOAction action
