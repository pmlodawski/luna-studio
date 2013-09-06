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


type Pass   state m     = (Functor m, MonadState state m, LogWriter m)
type Result input state = (Maybe input, state, LogList)

run f state = runRWS (runMaybeT f) 0 state

runNested f state = do
    let (nast, _, logs) = run f state
    Logger.append logs
    hoistMaybe nast