---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------
{-# LANGUAGE FlexibleContexts, NoMonomorphismRestriction, ConstraintKinds, TupleSections #-}

module Flowbox.Luna.Passes.Pass where

import           Control.Monad.State          
import           Control.Monad.Writer         
import           Control.Monad.RWS            
import           Control.Monad.Trans.Maybe    
import           Control.Monad.Trans.Either   
import           Control.Error                

import           Flowbox.System.Log.Logger    
import qualified Flowbox.System.Log.Logger  as Logger

import           Prelude                    hiding (fail)
import qualified Prelude                    as Prelude


type PassMonad   s m      = (Functor m, MonadState s m, LogWriter m)
type Transformer s a m b  = EitherT a (RWS [Int] LogList s) b -> EitherT a m b
type Result      m output = EitherT String m output

data NoState = NoState deriving (Show)


run :: state -> EitherT a (RWS [Int] LogList state) b -> (Either a b, state, LogList)
run state f = runRWS (runEitherT f) [] state


runM :: PassMonad s m => state -> Transformer state a m b
runM state f = do
    let (nast, _, logs) = run state f
    Logger.append logs
    hoistEither nast


fail :: Monad m => EitherT String m a
fail = left "Pass failed"