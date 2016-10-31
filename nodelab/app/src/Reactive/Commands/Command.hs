{-# LANGUAGE TypeFamilies #-}

module Reactive.Commands.Command where

import Utils.PreludePlus
import Control.Monad.State
import Control.Monad.Writer
import Control.Lens.Internal.Zoom


newtype IOAction = IOAction { unIOAction :: IO () }

instance Monoid IOAction where
    mempty = IOAction $ return ()
    mappend (IOAction a) (IOAction b) = IOAction $ a >> b

newtype Command a b = Command { unCommand :: StateT a (Writer IOAction) b }
                    deriving (Functor, Applicative, Monad, MonadWriter IOAction, MonadState a)

type instance Zoomed (Command a) = Focusing (Writer IOAction)

instance Zoom (Command s) (Command t) s t where
    zoom l (Command m) = Command (zoom l m)

command :: (a -> (IO (), a)) -> Command a ()
command f = do
    (action, st) <- gets f
    performIO action
    put st

pureCommand :: (a -> a) -> Command a ()
pureCommand = modify

ioCommand :: (a -> IO ()) -> Command a ()
ioCommand f = gets f >>= performIO

runCommand :: Command a b -> a -> (b, IO (), a)
runCommand cmd st = case runWriter (runStateT (unCommand cmd) st) of
    ((res, st'), IOAction act) -> (res, act, st')

execCommand :: Command a b -> a -> (IO (), a)
execCommand cmd st = case runCommand cmd st of
    (_, act, st') -> (act, st')

performIO :: IO () -> Command a ()
performIO action = tell $ IOAction action
