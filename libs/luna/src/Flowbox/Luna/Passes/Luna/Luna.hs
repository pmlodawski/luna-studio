---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------
{-# LANGUAGE FlexibleContexts, NoMonomorphismRestriction, ConstraintKinds, TupleSections #-}

module Flowbox.Luna.Passes.Luna.Luna where

import qualified Flowbox.Luna.Passes.Pass    as Pass
import           Flowbox.System.Log.Logger     
import qualified Flowbox.System.Log.Logger   as Logger

import           Control.Monad.RWS            
import           Control.Monad.Trans.Either   

import qualified Prelude                     as Prelude
import           Prelude                     hiding (error)



run :: EitherT a (RWS [Int] LogList Pass.NoState) b -> IO (Either a b)
run f = do
	let (result, _, logs) = Pass.run Pass.NoState f
	Logger.logsIO logs
	return result
