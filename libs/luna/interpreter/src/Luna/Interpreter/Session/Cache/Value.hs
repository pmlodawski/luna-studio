---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Flowbox Team <contact@flowbox.io>, 2014
-- Proprietary and confidential
-- Unauthorized copying of this file, via any medium is strictly prohibited
---------------------------------------------------------------------------
{-# LANGUAGE LambdaCase      #-}
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
import qualified Luna.Graph.Flags                            as Flags
import qualified Luna.Interpreter.Session.Cache.Cache        as Cache
import qualified Luna.Interpreter.Session.Cache.Info         as CacheInfo
import qualified Luna.Interpreter.Session.Cache.Status       as Status
import qualified Luna.Interpreter.Session.Data.CallPoint     as CallPoint
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
    varName   <- foldedReRoute callPointPath
    cacheInfo <- Cache.getCacheInfo callPointPath
    let status = cacheInfo ^. CacheInfo.status
    assertE (status == Status.Ready) $ Error.CacheError $(loc) $ concat ["Object ", show callPointPath, " is not computed yet."]
    get varName callPointPath


data Status = Ready
            | Modified
            | NonCacheable
            | NotInCache
            | Unknown
            deriving (Show, Eq)


getWithStatus :: CallPointPath -> Session (Status, Maybe Value)
getWithStatus callPointPath = do
    varName <- foldedReRoute callPointPath
    Env.cachedLookup callPointPath >>= \case
        Nothing        -> return (NotInCache, Nothing)
        Just cacheInfo -> do
            allReady <- Env.getAllReady
            let returnBytes status = do
                    value <- get varName callPointPath
                    return (status, value)
                returnNothing status = return (status, Nothing)

            case (cacheInfo ^. CacheInfo.status, allReady) of
                (Status.Ready,        True ) -> returnBytes   Ready
                (Status.Ready,        False) -> returnBytes   Unknown
                (Status.Modified,     _    ) -> returnBytes   Modified
                (Status.Affected,     _    ) -> returnBytes   Modified
                (Status.NonCacheable, _    ) -> returnNothing NonCacheable


reportIfVisible :: CallPointPath -> Session ()
reportIfVisible callPointPath = do
    flags <- Env.getFlags $ last callPointPath
    unless (Flags.isSet' flags (view Flags.defaultNodeGenerated)
         || Flags.isSet' flags (view Flags.graphViewGenerated  )
         || Flags.isFolded flags                               ) $
        foldedReRoute callPointPath >>= report callPointPath


report :: CallPointPath -> VarName -> Session ()
report callPointPath varName = do
    resultCB  <- Env.getResultCallBack
    projectID <- Env.getProjectID
    result    <- get varName callPointPath
    safeLiftIO' (Error.CallbackError $(loc)) $ resultCB projectID callPointPath result


get :: VarName -> CallPointPath -> Session (Maybe Value)
get varName callPointPath = do
    mode <- Env.getSerializationMode callPointPath
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


foldedReRoute :: CallPointPath -> Session VarName
foldedReRoute callPointPath = do
    let callPointLast = last callPointPath
        callPointInit = init callPointPath
    mfoldTop <- Flags.getFoldTop <$> Env.getFlags callPointLast
    let newCallPointPath = case mfoldTop of
            Nothing     -> callPointPath
            Just nodeID -> callPointInit
                        ++ [callPointLast & CallPoint.nodeID .~ nodeID]
    cacheInfo <- Cache.getCacheInfo newCallPointPath
    return $ cacheInfo ^. CacheInfo.recentVarName
