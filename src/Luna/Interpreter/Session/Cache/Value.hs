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
import qualified Data.Set            as Set
import qualified GHC

import           Flowbox.Control.Error
import qualified Flowbox.Data.Error                          as ValueError
import qualified Flowbox.Data.Serialization                  as Serialization
import           Flowbox.Prelude
import           Flowbox.Source.Location                     (loc)
import           Flowbox.System.Log.Logger                   as L
import           Generated.Proto.Mode.ModeValue              (ModeValue (ModeValue))
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
import qualified Luna.Interpreter.Session.TargetHS.Bindings  as Bindings



logger :: LoggerIO
logger = getLoggerIO $(moduleName)


getIfReady :: CallPointPath -> Session [ModeValue]
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


getWithStatus :: CallPointPath -> Session (Status, [ModeValue])
getWithStatus callPointPath = do
    varName <- foldedReRoute callPointPath
    Env.cachedLookup callPointPath >>= \case
        Nothing        -> return (NotInCache, [])
        Just cacheInfo -> do
            allReady <- Env.getAllReady
            let returnBytes status = do
                    value <- get varName callPointPath
                    return (status, value)
                returnNothing status = return (status, [])

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
    results   <- get varName callPointPath
    safeLiftIO' (Error.CallbackError $(loc)) $
        resultCB projectID callPointPath results


get :: VarName -> CallPointPath -> Session [ModeValue]
get varName callPointPath = do
    modes <- Env.getSerializationModes callPointPath
    let tmpName = "_tmp"
        toValueExpr = "toValue " ++ tmpName
        computeExpr = concat [tmpName, " <- return $ compute ", varName, " def"]

        excHandler :: Catch.SomeException -> MGHC.Ghc [ModeValue]
        excHandler exc = do
            logger L.error $ show exc
            val <- liftIO (Serialization.toValue (ValueError.Error $ show exc) def)
            return $ map (\mode -> ModeValue mode val) $ Set.toList modes

    Session.withImports [ "Flowbox.Data.Serialization"
                        , "Flowbox.Data.Mode"
                        , "Flowbox.Graphics.Serialization"
                        , "Prelude"
                        , "Generated.Proto.Data.Value" ]
                        $ lift2 $ flip Catch.catch excHandler $ do
        logger trace computeExpr
        Bindings.remove tmpName
        _      <- GHC.runStmt computeExpr GHC.RunToCompletion
        action <- HEval.interpret toValueExpr
        Bindings.remove tmpName
        liftIO $ mapM (\mode -> ModeValue mode <$> action mode) $ Set.toList modes


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
