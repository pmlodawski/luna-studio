---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE TupleSections #-}

module Flowbox.Bus.RPC.RPC where

import Control.Exception          (SomeException, try)
import Control.Monad.Trans.Either
import Control.Monad.Trans.State

import Flowbox.Control.Error hiding (err)
import Flowbox.Prelude



type RPC s m a = EitherT Error (StateT s m) a

type Error = String


data NoState = NoState
             deriving (Read, Show)


run :: MonadIO m => RPC s IO r -> StateT s m (Either Error r)
run rpc = do
    s <- get
    result <- liftIO $ (try :: IO a -> IO (Either SomeException a)) $ runStateT (runEitherT rpc) s
    case result of
        Left err        -> return $ Left $ "Unhandled exception: " ++ show err
        Right (res, s') -> case res of
            Left err -> return $ Left err
            Right r  -> do put s'
                           return $ Right r


runLifted :: Monad m => RPC s m r -> StateT s m (Either Error r)
runLifted = runEitherT

