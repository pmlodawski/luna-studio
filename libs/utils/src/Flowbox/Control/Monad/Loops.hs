---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Flowbox Team <contact@flowbox.io>, 2014
-- Proprietary and confidential
-- Unauthorized copying of this file, via any medium is strictly prohibited
---------------------------------------------------------------------------
module Flowbox.Control.Monad.Loops (
    module X,
    repeatUntil,
    untilRight,
) where

import Control.Monad       (liftM)
import Control.Monad.Loops as X

import Flowbox.Prelude



repeatUntil :: Monad m => m a -> (a -> Bool) -> m [a]
repeatUntil action predicate = do
    result <- action
    if predicate result
        then liftM ((:) result) (repeatUntil action predicate)
        else return [result]


untilRight :: Monad m => m (Either a b) -> (a -> m ()) -> m b
untilRight action errorHandler = do
    result <- action
    case result of
        Right ok -> return ok
        Left err -> do errorHandler err
                       untilRight action errorHandler
