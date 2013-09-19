---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------
{-# LANGUAGE FlexibleContexts, ConstraintKinds #-}

module Flowbox.Luna.Passes.CabalGen.CabalGen where

import           Control.Monad.RWS                          

import           Flowbox.Prelude                            
import           Flowbox.Luna.Passes.CabalGen.CabalConfig   (CabalConfig)
import qualified Flowbox.System.UniPath                   as UniPath
import qualified Flowbox.Luna.Passes.CabalGen.Defaults    as Defaults
import qualified Flowbox.Luna.Passes.Pass                 as Pass
import           Flowbox.Luna.Passes.Pass                   (PassMonad)



run :: PassMonad s m => String -> Pass.Result m CabalConfig
run = return . genCabal 


genCabal :: String -> CabalConfig
genCabal name = Defaults.defaultConfig name [hsSourceDir] mainIs where
    hsSourceDir = UniPath.fromUnixString "src"
    mainIs      = UniPath.fromUnixString "Main.hs"
