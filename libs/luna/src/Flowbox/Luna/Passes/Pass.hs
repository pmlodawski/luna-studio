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

import           Prelude                    hiding(fail)
import qualified Prelude                    as Prelude


type Pass   state m      = (Functor m, MonadState state m, LogWriter m)
type Result output state = (Either String output, state, LogList)

run state f = runRWS (runEitherT f) 0 state

apply state f inputs = case inputs of
                           Left  e -> Prelude.fail e
                           Right v -> return $ run state (f v)

runNested state f = do
    let (nast, _, logs) = run state f
    Logger.append logs
    hoistEither nast

fail :: Monad m => EitherT String m a
fail = left "Pass failed"