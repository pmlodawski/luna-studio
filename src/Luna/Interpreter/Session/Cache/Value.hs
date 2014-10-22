---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Flowbox Team <contact@flowbox.io>, 2014
-- Proprietary and confidential
-- Unauthorized copying of this file, via any medium is strictly prohibited
---------------------------------------------------------------------------
{-# LANGUAGE TemplateHaskell #-}

module Luna.Interpreter.Session.Cache.Value where

import qualified Control.Monad.Catch as Catch
import qualified Control.Monad.Ghc   as MGHC
import qualified GHC

import           Flowbox.Control.Error
import qualified Flowbox.Data.Error                          as ValueError
import qualified Flowbox.Data.Serialization                  as Serialization
import           Flowbox.Prelude
import           Flowbox.Source.Location                     (loc)
import           Flowbox.System.Log.Logger
import           Generated.Proto.Data.Value                  (Value)
import qualified Luna.Interpreter.Session.Cache.Cache        as Cache
import qualified Luna.Interpreter.Session.Cache.Info         as CacheInfo
import qualified Luna.Interpreter.Session.Cache.Status       as Status
import           Luna.Interpreter.Session.Data.CallPointPath (CallPointPath)
import           Luna.Interpreter.Session.Data.VarName       (VarName)
import qualified Luna.Interpreter.Session.Env                as Env
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

            allReady <- Env.getAllReady
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
    resultCB  <- Env.getResultCallBack
    projectID <- Env.getProjectID
    result    <- get varName
    safeLiftIO' (Error.CallbackError $(loc)) $ resultCB projectID callPointPath result


get :: VarName -> Session (Maybe Value)
get varName = do
    mode <- Env.getSerializationMode
    let toValueExpr = "toValue " ++ varName
        computeExpr = concat [varName, " <- return $ compute ", varName, " def"]

        excHandler :: Catch.SomeException -> MGHC.Ghc (Maybe Value)
        excHandler exc = do
            logger warning $ show exc
            liftIO $ Serialization.toValue (ValueError.Error $ show exc) mode
    Session.withImports [ "Flowbox.Data.Serialization"
                        , "Flowbox.Data.Mode"
                        , "Flowbox.Graphics.Serialization"
                        , "Prelude"
                        , "Generated.Proto.Data.Value" ]
                        $ lift2 $ flip Catch.catch excHandler $ do
        logger trace computeExpr
        _      <- GHC.runStmt computeExpr GHC.RunToCompletion
        action <- HEval.interpret toValueExpr
        liftIO $ action mode
