---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------
{-# LANGUAGE FlexibleContexts, NoMonomorphismRestriction, ConstraintKinds, TupleSections #-}

module Flowbox.Luna.Passes.Pass where

import           Flowbox.Prelude              
import           Control.Monad.State          

import           Control.Monad.RWS            hiding(state)
import           Control.Monad.Trans.Either   

import           Flowbox.System.Log.Logger    
import qualified Flowbox.System.Log.Logger  as Logger

import           Prelude                    hiding (fail)
import qualified Prelude                    as Prelude

type PassError              = String

type PassMonad    s m       = (Functor m, MonadState s m, LogWriter m)
type Transformer  s b       = EitherT PassError (RWS [Int] LogList s) b 
type Result       m output  = EitherT PassError m output
type TransformerT s a m b   = EitherT a (RWST [Int] LogList s m) b -> m (Either a b)  -- do not use

data NoState = NoState deriving (Show)


--run :: state -> EitherT a (RWS [Int] LogList state) b -> (Either a b, state, LogList)
--run :: (PassMonad s m, MonadWriter w0 m)=> s -> EitherT e0 (RWS [a1] w0 s) a0 -> EitherT e0 m (a0, s)
--run :: PassMonad outstate m => state -> Transformer state a m b
run ::  (MonadWriter LogList m) => state -> Transformer state b -> Result m (b, state)
run s f = do
    let (result, state, logs) = runRWS (runEitherT f) [] s
    Logger.append logs
    let out = case result of
                  Left  e  -> Left e
                  Right r  -> Right (r, state)
    hoistEither out 

--run :: state -> EitherT a (RWS [Int] LogList state) b -> (Either a b, state, LogList)
runT s f = runRWST (runEitherT f) [] s


run_ :: PassMonad outstate m => state -> Transformer state b -> Result m b
run_ s f = do
    (result, _) <- run s f
    return result


run'_ :: PassMonad outstate m => state -> Transformer state b -> Result m state
run'_ s f = do
    (_, state) <- run s f
    return state


fail :: Monad m => String -> EitherT PassError m a
fail = left