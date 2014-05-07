---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
module Flowbox.Control.Error (
    module Control.Error,
    runScript,
    (<?>),
    assert,
    safeLiftIO,
    safeLiftIO',
    eitherToM,
    eitherStringToM,
    liftIO,
) where

import           Control.Error          hiding (runScript)
import           Control.Exception      (Exception)
import qualified Control.Exception      as Exc
import           Control.Monad.IO.Class (MonadIO, liftIO)

import Flowbox.Prelude



runScript :: Script a -> IO a
runScript s = do
    e <- runEitherT s
    case e of
        Left  m -> fail m
        Right a -> return a

infixl 4 <?>
(<?>) :: Monad m => Maybe b -> String -> m b
val <?> m = case val of
    Just v  -> return v
    Nothing -> fail m


assert :: Monad m => Bool -> String -> m ()
assert condition msg = if condition
    then return ()
    else fail msg


safeLiftIO :: IO b -> EitherT String IO b
safeLiftIO operation = do
    result <- liftIO $ (Exc.try :: IO a -> IO (Either Exc.SomeException a)) operation
    hoistEither $ fmapL show result


safeLiftIO' :: (Exception e, Show e) => (e -> a) -> IO b -> EitherT a IO b
safeLiftIO' excMap operation  = do
    result <- liftIO $ Exc.try operation
    hoistEither $ fmapL excMap result


eitherToM :: (MonadIO m, Show a) => Either a b -> m b
eitherToM = either (fail . show) return


eitherStringToM :: MonadIO m => Either String b -> m b
eitherStringToM = either fail return
