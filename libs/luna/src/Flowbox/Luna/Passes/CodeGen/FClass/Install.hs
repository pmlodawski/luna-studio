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
import           Flowbox.Luna.Passes.Analysis.FuncPool.Pool   (Pool(Pool))
import qualified Flowbox.Luna.Passes.CodeGen.FClass.Gen     as FClassGen
import qualified Flowbox.Luna.Passes.Pass                   as Pass
import           Flowbox.Luna.Passes.Pass                     (PassMonadIO)
import           Flowbox.System.Log.Logger                    
import           Flowbox.System.UniPath                       (UniPath)

loggerIO :: LoggerIO
loggerIO = getLoggerIO "Flowbox.Luna.Passes.CodeGen.FClass.Install"


-----------------------------------

--run :: MonadIO m => String -> UniPath -> m ()
--run = genAndInstall


run :: PassMonadIO s m  => UniPath -> Pool -> Pass.Result m ()
run cabalDevPath (Pool names) = do
    mapM_ (FClassGen.genAndInstall cabalDevPath) $ Set.toList names
