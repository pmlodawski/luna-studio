{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}

module Logger.Internal.LoggerT where

import Data.Monoid            (Monoid(..))
import Data.Functor.Identity  (Identity)

import Control.Applicative    (Applicative(..),Alternative(..))

import Control.Monad          (MonadPlus(..),ap,liftM)
import Control.Monad.Except   (MonadError(..))
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Trans    (MonadTrans(..))


import Logger.Internal.Stack



newtype LoggerT e m a = LoggerT { runLoggerT :: m (Either e a, [Log e]) }
type Logger e = LoggerT e Identity


instance (Monad m) => Monad (LoggerT e m) where
  return x = LoggerT $ return (Right x, [])
  m >>= f = LoggerT $ do
    v <- runLoggerT m
    case v of
      (Left l, s) -> return (Left l, s)
      (Right r, s) -> do (mb, s') <- runLoggerT (f r)
                         return (mb, s++s')

instance MonadTrans (LoggerT e) where
  lift ma = LoggerT $ do
    res <- ma
    return (Right res, [])

instance (MonadIO m) => MonadIO (LoggerT e m) where
  liftIO mio = LoggerT $ do
    res <- liftIO mio
    return (Right res, [])

instance (Monad m) => Functor (LoggerT e m) where
  fmap = liftM

instance (Monad m) => Applicative (LoggerT e m) where
  pure = return
  (<*>) = ap

instance (Monad m, Show e) => MonadError e (LoggerT e m) where
  throwError e = LoggerT $ return (Left e, [Error $ show e])
  ma `catchError` handler = LoggerT $ do
    (mres, s) <- runLoggerT ma
    case mres of
      Left l -> runLoggerT $ handler l
      _      -> return (mres, s)

instance (Monad m, Show e, Monoid e) => Alternative (LoggerT e m) where
  empty = mzero
  (<|>) = mplus

instance (Monad m, Show e, Monoid e) => MonadPlus (LoggerT e m) where
  mzero = LoggerT $ return (Left mempty, [])
  mplus ma mb = ma `catchError` const mb
