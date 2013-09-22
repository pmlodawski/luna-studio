---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------
{-# LANGUAGE FlexibleContexts, ConstraintKinds #-}

module Flowbox.Luna.Passes.CodeGen.Cabal.Gen where

--import           Control.Monad.RWS                  

--import           Flowbox.Prelude                    
--import           Flowbox.Luna.Data.Cabal.Config     (Config)
--import qualified Flowbox.System.UniPath           as UniPath
--import qualified Flowbox.Luna.Data.Cabal.Defaults as Defaults
--import qualified Flowbox.Luna.Passes.Pass         as Pass
--import           Flowbox.Luna.Passes.Pass           (PassMonad)



--run :: PassMonad s m => String -> Pass.Result m Config
--run = return . genCabal 


--genCabal :: String -> Config
--genCabal name = Defaults.defaultConfig name [hsSourceDir] mainIs where
--    hsSourceDir = UniPath.fromUnixString "src"
--    mainIs      = UniPath.fromUnixString "Main.hs"
