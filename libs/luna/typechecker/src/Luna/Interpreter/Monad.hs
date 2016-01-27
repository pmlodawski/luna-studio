{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE UndecidableInstances   #-}

module Luna.Interpreter.Monad where

import Prologue

import qualified Control.Monad.Catch      as Catch
import qualified Control.Monad.State      as State
import qualified Language.Haskell.Session as HS


-- TODO: template haskellize
-- >->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->

newtype InterpreterT env m a = InterpreterT { fromInterpreterT :: State.StateT env m a }
                               deriving (Functor, Monad, Applicative, MonadIO, MonadPlus, MonadTrans, Alternative, MonadFix, HS.GhcMonad, HS.ExceptionMonad, HS.HasDynFlags, Catch.MonadMask, Catch.MonadCatch, Catch.MonadThrow)

type Interpreter env = InterpreterT env Identity

class Monad m => InterpreterMonad env m | m -> env where
    get :: m env
    put :: env -> m ()

instance Monad m => InterpreterMonad env (InterpreterT env m) where
    get = InterpreterT State.get
    put = InterpreterT . State.put

instance State.MonadState s m => State.MonadState s (InterpreterT env m) where
    get = InterpreterT (lift State.get)
    put = InterpreterT . lift . State.put

instance {-# OVERLAPPABLE #-} (InterpreterMonad env m, MonadTrans t, Monad (t m)) => InterpreterMonad env (t m) where
    get = lift get
    put = lift . put

runT  ::            InterpreterT env m a -> env -> m (a, env)
evalT :: Monad m => InterpreterT env m a -> env -> m a
execT :: Monad m => InterpreterT env m a -> env -> m env

runT  = State.runStateT  . fromInterpreterT
evalT = State.evalStateT . fromInterpreterT
execT = State.execStateT . fromInterpreterT


run  :: Interpreter env a -> env -> (a, env)
eval :: Interpreter env a -> env -> a
exec :: Interpreter env a -> env -> env

run   = runIdentity .: runT
eval  = runIdentity .: evalT
exec  = runIdentity .: execT

with :: InterpreterMonad env m => (env -> env) -> m b -> m b
with f m = do
    s <- get
    put $ f s
    out <- m
    put s
    return out

modify :: InterpreterMonad env m => (env -> (env, a)) -> m a
modify = modifyM . fmap return

modify2 :: InterpreterMonad env m => (env -> (a, env)) -> m a
modify2 = modifyM2 . fmap return

modifyM :: InterpreterMonad env m => (env -> m (env, a)) -> m a
modifyM f = do
    s <- get
    (s', a) <- f s
    put $ s'
    return a

modifyM2 :: InterpreterMonad env m => (env -> m (a, env)) -> m a
modifyM2 f = do
    s <- get
    (a, s') <- f s
    put $ s'
    return a

modify_ :: InterpreterMonad env m => (env -> env) -> m ()
modify_ = modify . fmap (,())

-- <-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<

withEnv :: InterpreterMonad env m => (env -> (env, a)) -> m a
withEnv = withEnvM . fmap return

withEnv' :: InterpreterMonad env m => (env -> (a, env)) -> m a
withEnv' = withEnvM . fmap (return . switch')

withEnv_ :: InterpreterMonad env m => (env -> env) -> m ()
withEnv_ = withEnv . fmap (,())

withEnvM :: InterpreterMonad env m => (env -> m (env, a)) -> m a
withEnvM = modifyM

withEnvM_ :: InterpreterMonad env m => (env -> m env) -> m ()
withEnvM_ = withEnvM . (fmap . fmap) (,())

runInterpreterT  :: Functor m => InterpreterT env m a -> env -> m (a, env)
execInterpreterT :: Monad   m => InterpreterT env m a -> env -> m env
evalInterpreterT :: Monad   m => InterpreterT env m a -> env -> m a

runInterpreterT  = runT
execInterpreterT = execT
evalInterpreterT = evalT




switch' (a,b) = (b,a)
