---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Flowbox Team <contact@flowbox.io>, 2014
-- Proprietary and confidential
-- Unauthorized copying of this file, via any medium is strictly prohibited
---------------------------------------------------------------------------
module Luna.Interpreter.Session.Cache.Value where

import           Data.Typeable                (Typeable)
import qualified Language.Haskell.Interpreter as Interpreter

import           Flowbox.Control.Error
import           Flowbox.Prelude
import           Flowbox.System.Log.Logger
import qualified Luna.Interpreter.Session.Cache.Cache        as Cache
import qualified Luna.Interpreter.Session.Cache.Info         as CacheInfo
import qualified Luna.Interpreter.Session.Cache.Status       as Status
import           Luna.Interpreter.Session.Data.CallPointPath (CallPointPath)
import           Luna.Interpreter.Session.Session            (Session)



logger :: LoggerIO
logger = getLoggerIO "Luna.Interpreter.Session.Cache.Value"


get :: Typeable a => CallPointPath -> a -> Session a
get callPointPath witness = do
    cacheInfo <- Cache.lookupCacheInfo callPointPath
             <??&>  "Cache.Value.get : Object " ++ show callPointPath ++ " is not in cache."
    let varName = cacheInfo ^. CacheInfo.recentVarName
        status  = cacheInfo ^. CacheInfo.status

    assertE (status == Status.Ready) $ "Cache.Value.get : Object " ++ show callPointPath ++ " is not computed yet."
    lift2 $ Interpreter.interpret varName witness

