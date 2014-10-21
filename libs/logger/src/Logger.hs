module Logger (
    LoggerT, Logger,
    function, functionResult, trace, traceInline, err,
    formatStack,
    runLogger, runLoggerT,
    evalLogger, evalLoggerT,
    isFail, isFine
  ) where

import Data.Either             (isRight)

import Logger.Internal.LoggerT
import Logger.Internal.Runners
import Logger.Internal.Stack
import Logger.Internal.TraceInline (traceInline)



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

