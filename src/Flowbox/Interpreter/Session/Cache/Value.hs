---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Flowbox Team <contact@flowbox.io>, 2014
-- Proprietary and confidential
-- Unauthorized copying of this file, via any medium is strictly prohibited
---------------------------------------------------------------------------
module Flowbox.Interpreter.Session.Cache.Value where

import           Data.Typeable                (Typeable)
import qualified Language.Haskell.Interpreter as Interpreter

import           Flowbox.Control.Error
import qualified Flowbox.Interpreter.Session.Cache.Cache        as Cache
import qualified Flowbox.Interpreter.Session.Cache.Info         as CacheInfo
import qualified Flowbox.Interpreter.Session.Cache.Status       as Status
import           Flowbox.Interpreter.Session.Data.CallPointPath (CallPointPath)
import           Flowbox.Interpreter.Session.Session            (Session)
import           Flowbox.Prelude
import           Flowbox.System.Log.Logger



logger :: LoggerIO
logger = getLoggerIO "Flowbox.Interpreter.Session.Cache.Value"


get :: Typeable a => CallPointPath -> a -> Session a
get callPointPath witness = do
    cacheInfo <- Cache.lookupCacheInfo callPointPath
             <??&>  "Cache.Value.get : Object " ++ show callPointPath ++ " is not in cache."
    let varName = cacheInfo ^. CacheInfo.recentVarName
        status  = cacheInfo ^. CacheInfo.status

    assertE (status == Status.Ready) $ "Cache.Value.get : Object " ++ show callPointPath ++ " is not computed yet."
    lift2 $ Interpreter.interpret varName witness

