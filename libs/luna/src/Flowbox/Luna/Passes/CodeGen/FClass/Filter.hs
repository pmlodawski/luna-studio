---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------
{-# LANGUAGE ConstraintKinds, FlexibleContexts #-}

module Flowbox.Luna.Passes.CodeGen.FClass.Filter where

import           Control.Monad.RWS                            
import qualified Data.List.Split                            as Split
import qualified Data.Map                                   as Map
import qualified Data.Set                                   as Set

import           Flowbox.Prelude                            hiding (error)
import           Flowbox.Luna.Passes.Analysis.FuncPool.Pool   (Pool(Pool))
import qualified Flowbox.Luna.Passes.CodeGen.FClass.Gen     as FClassGen
import qualified Flowbox.Luna.Passes.Pass                   as Pass
import           Flowbox.Luna.Passes.Pass                     (PassMonadIO)
import           Flowbox.System.Log.Logger                    
import qualified Flowbox.System.Process                     as Process
import           Flowbox.System.UniPath                       (UniPath)


loggerIO :: LoggerIO
loggerIO = getLoggerIO "Flowbox.Luna.Passes.CodeGen.FClass.Filter"


run :: PassMonadIO s m  => UniPath -> Pool -> Pass.Result m Pool
run cabalDevPath (Pool names) = liftIO $ do
    output <- Process.readProcessInFolder cabalDevPath "cabal-dev" ["ghc-pkg", "list", "--simple-output", "--names-only"] ""
    let installed = Set.fromList $ Split.splitOn " " output
        namesList = Set.toList names
        namesMap  = Map.fromList $ zip (map FClassGen.packageName $ namesList) namesList
        toInstallMap = Map.filterWithKey (\k _ -> Set.notMember k installed) namesMap
    return $ Pool $ Set.fromList $ Map.elems toInstallMap
