---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------

{-# LANGUAGE ConstraintKinds           #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TupleSections             #-}

module Flowbox.Luna.Passes.General.Luna.Luna where

import Control.Monad.Trans.Either

import           Flowbox.Control.Error     (eitherStringToM)
import qualified Flowbox.Luna.Passes.Pass  as Pass
import           Flowbox.Prelude           hiding (error)
import qualified Flowbox.System.Log.Logger as Logger



--run :: Pass.TransformerT Pass.NoState String IO b -> IO (Either Pass.PassError b)
run group = runEitherT group


runIO f = eitherStringToM =<< f
--runIO :: Pass.TransformerT Pass.NoState String IO b -> IO b
--runIO f = either2io =<< (run . hoistEither) =<< f



