---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Flowbox Team <contact@flowbox.io>, 2014
-- Proprietary and confidential
-- Unauthorized copying of this file, via any medium is strictly prohibited
---------------------------------------------------------------------------
module Luna.Interpreter.Session.Cache.Free where

import qualified Data.Map as Map

import           Flowbox.Prelude
import           Luna.Interpreter.Session.Cache.Info   (CacheInfo)
import qualified Luna.Interpreter.Session.Cache.Info   as CacheInfo
import           Luna.Interpreter.Session.Data.VarName (VarName)
import           Luna.Interpreter.Session.Session      (Session)
import qualified Luna.Interpreter.Session.Session      as Session



freeVarName :: VarName -> Session ()
freeVarName varName = Session.runAssignment varName "()"


freeCacheInfo :: CacheInfo -> Session ()
freeCacheInfo cacheInfo = do
    freeVarName $ cacheInfo ^. CacheInfo.recentVarName
    mapM_ freeVarName $ Map.elems $ cacheInfo ^. CacheInfo.dependencies
