---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Flowbox Team <contact@flowbox.io>, 2014
-- Proprietary and confidential
-- Unauthorized copying of this file, via any medium is strictly prohibited
---------------------------------------------------------------------------
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Flowbox.Control.Monad.Morph (
    module X,
) where

import Control.Monad.Morph        as X
import Control.Monad.Trans.Either



instance MFunctor (EitherT e) where
    hoist nat m = EitherT (nat (runEitherT m))
