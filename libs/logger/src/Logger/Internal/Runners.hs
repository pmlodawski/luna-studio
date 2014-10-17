module Logger.Internal.Runners where

import Control.Monad           (liftM)

import Data.Either             (isLeft,isRight)
import Data.Functor.Identity   (runIdentity)

import Logger.Internal.Stack
import Logger.Internal.LoggerT



isFine :: (Monad m) => LoggerT e m a -> LoggerT e m Bool
isFine logger = LoggerT $ do
  (ma, s) <- runLoggerT logger
  return (Right (isRight ma), s)

isFail :: (Monad m) => LoggerT e m a -> LoggerT e m Bool
isFail logger = LoggerT $ do
  (ma, s) <- runLoggerT logger
  return (Right (isLeft ma), s)

evalLoggerT :: (Monad m) => LoggerT e m a -> m (Either e a)
evalLoggerT = liftM fst . runLoggerT

runLogger :: Logger e a -> (Either e a, [Log e])
runLogger = runIdentity . runLoggerT

evalLogger :: Logger e a -> Either e a
evalLogger = runIdentity . evalLoggerT
