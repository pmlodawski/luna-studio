---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}

module Flowbox.PluginManager.Init.Remote where

import Control.Monad.Trans.Either

import qualified Flowbox.PluginManager.RPC.Client as Client
import           Flowbox.Prelude



init :: EitherT String IO ()
init = undefined
