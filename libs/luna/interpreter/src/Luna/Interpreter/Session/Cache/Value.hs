---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Flowbox Team <contact@flowbox.io>, 2014
-- Proprietary and confidential
-- Unauthorized copying of this file, via any medium is strictly prohibited
---------------------------------------------------------------------------
{-# LANGUAGE TemplateHaskell #-}

module Luna.Interpreter.Session.Cache.Value where

import qualified GHC

import           Flowbox.Control.Error
import           Flowbox.Prelude
import           Flowbox.Source.Location                     (loc)
import           Flowbox.System.Log.Logger
import           Generated.Proto.Data.Value                  (Value)
import qualified Luna.Interpreter.Session.Cache.Cache        as Cache
import qualified Luna.Interpreter.Session.Cache.Info         as CacheInfo
import qualified Luna.Interpreter.Session.Cache.Status       as Status
import           Luna.Interpreter.Session.Data.CallPointPath (CallPointPath)
import           Luna.Interpreter.Session.Data.VarName       (VarName)
import qualified Luna.Interpreter.Session.Error              as Error
import qualified Luna.Interpreter.Session.Hint.Eval          as HEval
import           Luna.Interpreter.Session.Session            (Session)
import qualified Luna.Interpreter.Session.Session            as Session



logger :: LoggerIO
logger = getLoggerIO $(moduleName)


getIfReady :: CallPointPath -> Session (Maybe Value)
getIfReady callPointPath = do
    cacheInfo <- Cache.lookupCacheInfo callPointPath
    let varName = cacheInfo ^. CacheInfo.recentVarName
        status  = cacheInfo ^. CacheInfo.status

    assertE (status == Status.Ready) $ Error.CacheError $(loc) $ concat ["Object ", show callPointPath, " is not computed yet."]
    get varName


data Status = Ready
            | Modified
            | NonCacheable
            | NotInCache
            | Unknown
            deriving (Show, Eq)


getWithStatus :: CallPointPath -> Session (Status, Maybe Value)
getWithStatus callPointPath = do
    mcacheInfo <- Cache.lookupCacheInfoMaybe callPointPath
    case mcacheInfo of
        Nothing        -> return (NotInCache, Nothing)
        Just cacheInfo -> do
            let varName = cacheInfo ^. CacheInfo.recentVarName

            allReady <- Session.getAllReady
            let returnBytes status = do
                    value <- get varName
                    return (status, value)
                returnNothing status = return (status, Nothing)

            case (cacheInfo ^. CacheInfo.status, allReady) of
                (Status.Ready,        True ) -> returnBytes   Ready
                (Status.Ready,        False) -> returnBytes   Unknown
                (Status.Modified,     _    ) -> returnBytes   Modified
                (Status.Affected,     _    ) -> returnBytes   Modified
                (Status.NonCacheable, _    ) -> returnNothing NonCacheable


report :: CallPointPath -> VarName -> Session ()
report callPointPath varName = do
    resultCB  <- Session.getResultCallBack
    projectID <- Session.getProjectID
    result    <- get varName
    safeLiftIO' (Error.CallbackError $(loc)) $ resultCB projectID callPointPath result


get :: VarName -> Session (Maybe Value)
get varName = do
    let expr = "toValue $ compute " ++ varName
        excHandler exc = do
            logger warning $ show exc
            return Nothing
    Session.withImports [ "Flowbox.Data.Serialization"
                        , "Flowbox.Graphics.Serialization"
                        , "Prelude"
                        , "Generated.Proto.Data.Value" ]
                        $ lift2 $ GHC.handleSourceError excHandler $ do
        --let computeExpr =  concat [varName, " = compute ", varName]
        --_      <- GHC.runDecls computeExpr
        action <- HEval.interpret expr
        liftIO action
