---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------
{-# LANGUAGE ConstraintKinds, FlexibleContexts #-}

module Flowbox.Luna.Passes.CodeGen.FClass.Install where


import qualified Data.Set                                   as Set

import           Flowbox.Prelude                            hiding (error)
import           Flowbox.Config.Config                        (Config)
import           Flowbox.Luna.Passes.Analysis.FuncPool.Pool   (Pool(Pool))
import qualified Flowbox.Luna.Passes.CodeGen.FClass.Gen     as FClassGen
import qualified Flowbox.Luna.Passes.Pass                   as Pass
import           Flowbox.Luna.Passes.Pass                     (PassMonadIO)



run :: PassMonadIO s m  => Config -> Pool -> [String] -> Pass.Result m ()
run config (Pool names) flags = do
    mapM_ (\name -> FClassGen.genAndInstall config name flags) $ Set.toList names
