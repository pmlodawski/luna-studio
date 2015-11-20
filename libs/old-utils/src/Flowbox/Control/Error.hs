---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE CPP #-}

module Flowbox.Control.Error (
  module Flowbox.Control.Error
, module X
, MonadIO
, liftIO
) where

#if MIN_VERSION_errors(2,0,0)
import Control.Error as X hiding (catchEither, runScript)
#else
import Control.Error              as X hiding (catchEither, runScript, throwE)
import Control.Monad.Trans.Either (bimapEitherT)
#endif
import qualified Control.Exception as Exc
import qualified Data.Maybe        as Maybe

import Flowbox.Prelude


mkExceptT :: Monad m => m (Either e a) -> ExceptT e m a
#if MIN_VERSION_errors(2,0,0)
mkExceptT = ExceptT
#else
mkExceptT = EitherT
type ExceptT = EitherT

runExceptT :: ExceptT e m a -> m (Either e a)
runExceptT = runEitherT

throwE :: Monad m => e -> ExceptT e m a
throwE = left


withExceptT :: Functor m => (e -> a) -> ExceptT e m b -> ExceptT a m b
withExceptT = flip bimapEitherT id

mapExceptT = mapEitherT

exceptT :: Monad m => (a -> m c) -> (b -> m c) -> ExceptT a m b -> m c
exceptT = eitherT
#endif

{-# DEPRECATED runScript "" #-}
runScript :: Script a -> IO a
runScript s = do
    e <- runExceptT s
    case e of
        Left  m -> fail m
        Right a -> return a

infixl 4 <?.>
(<?.>) :: Monad m => Maybe b -> String -> m b
val <?.> m = Maybe.maybe (fail m) return val


infixl 4 <??&.>
(<??&.>) :: Monad m => m (Maybe b) -> String -> m b
val <??&.> m = Maybe.maybe (fail m) return =<< val


infixl 4 <?>
(<?>) :: Maybe b -> a -> Either a b
val <?> m = Maybe.maybe (Left m) Right val


infixl 4 <?&>
(<?&>) :: Either a (Maybe b) -> a -> Either a b
val <?&> m = Maybe.maybe (Left m) Right =<< val


infixl 4 <??>
(<??>) :: Monad m => Maybe b -> a -> ExceptT a m b
val <??> m = Maybe.maybe (throwE m) return val


infixl 4 <??&>
(<??&>) :: Monad m => ExceptT a m (Maybe b) -> a -> ExceptT a m b
val <??&> m = Maybe.maybe (throwE m) return =<< val


assertIO :: Monad m => Bool -> String -> m ()
assertIO condition msg = unless condition $ fail msg


assert :: Bool -> a -> Either a ()
assert condition msg = unless condition $ Left msg

assertE :: Monad m => Bool -> a -> ExceptT a m ()
assertE condition msg = unless condition $ throwE msg


-- FIXME [PM] : find better name
safeLiftIO :: MonadIO m => IO b -> ExceptT String m b
safeLiftIO = safeLiftIO' show


safeLiftIO' :: MonadIO m => (Exc.SomeException -> a) -> IO b -> ExceptT a m b
safeLiftIO' excMap operation  = do
    result <- liftIO $ Exc.try operation
    hoistEither $ fmapL excMap result


eitherToM :: (Monad m, Show a) => Either a b -> m b
eitherToM = either (fail . show) return


eitherToM' :: (Monad m, Show a) => m (Either a b) -> m b
eitherToM' action = action >>= eitherToM


eitherStringToM :: Monad m => Either String b -> m b
eitherStringToM = either fail return


eitherStringToM' :: Monad m => m (Either String b) -> m b
eitherStringToM' action = action >>= eitherStringToM

--TODO[PM]L rename to catchExceptT
catchEither :: (MonadTrans t, Monad (t m), Monad m)
            => (e -> t m b) -> ExceptT e m b -> t m b
catchEither handler fun = do
    result <- lift $ runExceptT fun
    case result of
        Left  e -> handler e
        Right r -> return r


hoistEitherWith :: Monad m => (e1 -> e) -> Either e1 a -> ExceptT e m a
hoistEitherWith conv = hoistEither . fmapL conv
