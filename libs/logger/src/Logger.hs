{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}

module Logger (
    LoggerT, Logger,
    function, functionResult, trace, err,
    formatStack,
    runLogger, runLoggerT,
    evalLogger, evalLoggerT,
    isFail, isFine
  ) where


import Data.Either            (isLeft,isRight)
import Data.Monoid            (Monoid(..))

import Data.Functor.Identity  (Identity,runIdentity)

import Control.Applicative    (Applicative(..),Alternative(..))

import Control.Monad          (MonadPlus(..),ap,join,liftM)

import Control.Monad.Except   (MonadError(..))
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Trans    (MonadTrans(..))


data Log e = CallResult String (Maybe String) [Log e]
           | Call       String Bool           [Log e]
           | Log        String
           | Error      String

instance Show e => Show (Log e) where
  showList = (++) . formatStack False
  show x = formatStack True [x]

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

formatStack :: (Show e) => Bool -> [Log e] -> String
formatStack fulldebug = concatMap (printStack 0)
  where printStack :: Int -> Log e -> String
        printStack n (CallResult str (Just a) stck2) = concat
          [ indent n
          , "CALL: "++str++" … -> "++show a++"\n"
          , if fulldebug then concatMap (printStack $ n+1) stck2 else ""
          ]
        printStack n (CallResult str Nothing stck2) = concat
          [ indent n
          , "CALL: "
          , str
          , " …\n"
          , concatMap (printStack $ n+1) stck2
          ]
        printStack n (Call str True stck2) = concat
          [ indent n
          , "CALL: "
          , str
          , "\n"
          , if fulldebug then concatMap (printStack $ n+1) stck2 else ""
          ]
        printStack n (Call str False stck2) = concat
          [ indent n
          , "CALL: "
          , str
          , "\n"
          , concatMap (printStack $ n+1) stck2
          ]
        printStack n (Log str) = concat
          [ indent n
          , "LOG : "
          , str
          , "\n"
          ]
        printStack n (Error str) = concat
          [ indent n
          , "ERR : "
          , str
          , "\n"
          ]
        indent n = if n>0
                 then join (replicate (n-1) "    ") ++ "  - "
                 else ""

function :: (Monad m) => String -> LoggerT e m r -> LoggerT e m r
function str mm = LoggerT $ do
  (ma, s) <- runLoggerT mm
  return (ma, [Call str (isRight ma) s])

functionResult :: (Monad m, Show r) => String -> LoggerT e m r -> LoggerT e m r
functionResult str mm = LoggerT $ do
  (ma, s) <- runLoggerT mm
  case ma of
    Right r -> return (ma, [CallResult str (Just $ show r) s])
    _       -> return (ma, [CallResult str Nothing s])

trace :: (Monad m) => String -> LoggerT e m ()
trace s = LoggerT $ return (Right (), [Log s])

err :: (Show e, Monad m) => e -> String -> LoggerT e m a
err e s = LoggerT $ return (Left e, [Error s])

