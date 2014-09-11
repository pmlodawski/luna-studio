---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Flowbox Team <contact@flowbox.io>, 2014
-- Proprietary and confidential
-- Unauthorized copying of this file, via any medium is strictly prohibited
---------------------------------------------------------------------------
module Luna.Interpreter.Session.Cache.Value where

import           Data.ByteString.Lazy         (ByteString)
import qualified Data.ByteString.Lazy.Char8   as ByteString
import qualified Language.Haskell.Interpreter as Interpreter

import           Flowbox.Control.Error
import           Flowbox.Prelude
import           Flowbox.System.Log.Logger
import qualified Luna.Interpreter.Session.Cache.Cache        as Cache
import qualified Luna.Interpreter.Session.Cache.Info         as CacheInfo
import qualified Luna.Interpreter.Session.Cache.Status       as Status
import           Luna.Interpreter.Session.Data.CallPointPath (CallPointPath)
import           Luna.Interpreter.Session.Data.VarName       (VarName)
import           Luna.Interpreter.Session.Session            (Session)
import qualified Luna.Interpreter.Session.Session            as Session



logger :: LoggerIO
logger = getLoggerIO "Luna.Interpreter.Session.Cache.Value"


getIfReady :: CallPointPath -> Session ByteString
getIfReady callPointPath = do
    cacheInfo <- Cache.lookupCacheInfo callPointPath
             <??&>  "Cache.Value.get : Object " ++ show callPointPath ++ " is not in cache."
    let varName = cacheInfo ^. CacheInfo.recentVarName
        status  = cacheInfo ^. CacheInfo.status

    assertE (status == Status.Ready) $ "Cache.Value.get : Object " ++ show callPointPath ++ " is not computed yet."
    get varName


report :: CallPointPath -> VarName -> Session ()
report callPointPath varName = do
    resultCB  <- Session.getResultCallBack
    projectID <- Session.getProjectID
    result    <- get varName
    safeLiftIO $ resultCB projectID callPointPath result


get :: VarName -> Session ByteString
get varName = do
    let expr = "show " ++ varName
    ByteString.pack <$> lift2 (Interpreter.interpret expr "")
