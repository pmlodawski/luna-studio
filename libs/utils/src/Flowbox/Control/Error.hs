---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------
module Flowbox.Control.Error (
    module Control.Error,
    runScript,
    (<?>),
    assert,
) where

import Control.Error hiding (runScript)

import Flowbox.Prelude


runScript :: Script a -> IO a
runScript s = do
    e <- runEitherT s
    case e of
        Left  m -> fail m
        Right a -> return a


(<?>) :: Monad m => Maybe b -> String -> m b
val <?> m = case val of
    Just v  -> return v
    Nothing -> fail m


assert :: Monad m => Bool -> String -> m ()
assert bool msg = if bool
    then return ()
    else fail msg

