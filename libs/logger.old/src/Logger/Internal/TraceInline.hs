{-# LANGUAGE FlexibleInstances, TypeFamilies #-}

module Logger.Internal.TraceInline (
    TraceInline,
    traceInline
  ) where

import Logger.Internal.LoggerT
import Logger.Internal.Stack

import Data.List



traceInline :: (TraceInline a) => a
traceInline = bar ([] :: [String])

class TraceInline a where
  bar :: [String] -> a

instance (Monad m, a~()) => TraceInline (LoggerT e m a) where
  bar = traceThat . intercalate " " . reverse
    where traceThat s = LoggerT $ return (Right (), [Log s])

instance (TraceInline r) => TraceInline (String -> r) where
  bar xs x = bar (x:xs)