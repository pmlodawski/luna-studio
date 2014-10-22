module Logger (
    LoggerT, Logger, Log,
    function, functionResult, trace, traceInline, err,
    formatStack,
    runLogger, runLoggerT,
    evalLogger, evalLoggerT,
    isFail, isFine
  ) where

import Data.List
import Data.Either             (isRight)

import Logger.Internal.LoggerT
import Logger.Internal.Runners
import Logger.Internal.Stack
import Logger.Internal.TraceInline (traceInline)



function :: (Monad m) => String -> [String] -> LoggerT e m r -> LoggerT e m r
function str args mm = LoggerT $ do
    (ma, s) <- runLoggerT mm
    return (ma, [Call funargs (isRight ma) s])
  where funargs = intercalate " " (str:"with args:":args)

functionResult :: (Monad m, Show r) => String -> [String] -> LoggerT e m r -> LoggerT e m r
functionResult str args mm = LoggerT $ do
    (ma, s) <- runLoggerT mm
    case ma of
      Right r -> return (ma, [CallResult funargs (Just $ show r) s])
      _       -> return (ma, [CallResult funargs Nothing s])
  where funargs = intercalate " " (str:"with args:":args)

trace :: (Monad m) => String -> LoggerT e m ()
trace s = LoggerT $ return (Right (), [Log s])

err :: (Show e, Monad m) => e -> String -> LoggerT e m a
err e s = LoggerT $ return (Left e, [Error s])

