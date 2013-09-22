---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------
{-# LANGUAGE FlexibleContexts, NoMonomorphismRestriction, ConstraintKinds, TupleSections #-}

module Flowbox.Luna.Passes.General.Luna.Luna where

import qualified Flowbox.Luna.Passes.Pass  as Pass
import qualified Flowbox.System.Log.Logger as Logger
import           Flowbox.Prelude           hiding (error)


run :: Pass.TransformerT Pass.NoState String IO b -> IO (Either Pass.PassError b)
run f = do
	(result, _, logs) <- Pass.runTRaw (Pass.Info "Luna") Pass.NoState f
	Logger.logsIO logs
	return result


