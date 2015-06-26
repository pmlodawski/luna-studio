---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Flowbox Team <contact@flowbox.io>, 2014
-- Proprietary and confidential
-- Unauthorized copying of this file, via any medium is strictly prohibited
---------------------------------------------------------------------------
{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE TemplateHaskell #-}

module Luna.Interpreter.Session.Cache.Value where

import           Control.Exception.Base (throw)
import           Control.Monad          (foldM)
import qualified Control.Monad.Catch    as Catch
import qualified Control.Monad.Ghc      as MGHC
import qualified Data.Map               as Map
import qualified Data.MultiSet          as MultiSet

import           Flowbox.Control.Error
import qualified Flowbox.Data.Error                           as ValueError
import qualified Flowbox.Data.Serialization                   as Serialization
import           Flowbox.Prelude
import           Flowbox.Source.Location                      (loc)
import           Flowbox.System.Log.Logger                    as L
import           Generated.Proto.Data.SValue                  (SValue)
import           Generated.Proto.Mode.Mode                    (Mode)
import           Generated.Proto.Mode.ModeValue               (ModeValue (ModeValue))
import qualified Luna.DEP.Graph.Flags                         as Flags
import qualified Luna.Interpreter.Session.Cache.Cache         as Cache
import           Luna.Interpreter.Session.Cache.Info          (CompValueMap)
import qualified Luna.Interpreter.Session.Cache.Info          as CacheInfo
import qualified Luna.Interpreter.Session.Cache.Status        as Status
import           Luna.Interpreter.Session.Data.AbortException (AbortException (AbortException))
import qualified Luna.Interpreter.Session.Data.CallPoint      as CallPoint
import           Luna.Interpreter.Session.Data.CallPointPath  (CallPointPath)
import           Luna.Interpreter.Session.Data.Time           (Time)
import           Luna.Interpreter.Session.Data.VarName        (VarName)
import qualified Luna.Interpreter.Session.Data.VarName        as VarName
import qualified Luna.Interpreter.Session.Env                 as Env
import qualified Luna.Interpreter.Session.Error               as Error
import qualified Luna.Interpreter.Session.Hint.Eval           as HEval
import           Luna.Interpreter.Session.Session             (Session)



logger :: LoggerIO
logger = getLoggerIO $moduleName


getIfReady :: CallPointPath -> Time -> Session mm [ModeValue]
getIfReady callPointPath time = do
    varName   <- getVarName callPointPath
    cacheInfo <- Cache.getCacheInfo callPointPath
    let status = cacheInfo ^. CacheInfo.status
    assertE (status == Status.Ready) $ Error.CacheError $(loc) $ concat ["Object ", show callPointPath, " is not computed yet."]
    get varName time callPointPath


data Status = Ready
            | Modified
            | NonCacheable
            | NotInCache
            | Unknown
            deriving (Show, Eq)


getWithStatus :: CallPointPath -> Time -> Session mm (Status, [ModeValue])
getWithStatus callPointPath time = do
    varName <- getVarName callPointPath
    Env.cachedLookup callPointPath >>= \case
        Nothing        -> return (NotInCache, [])
        Just cacheInfo -> do
            allReady <- Env.getAllReady
            let returnBytes status = do
                    value <- get varName time callPointPath
                    return (status, value)
                returnNothing status = return (status, [])

            case (cacheInfo ^. CacheInfo.status, allReady) of
                (Status.Ready,        True ) -> returnBytes   Ready
                (Status.Ready,        False) -> returnBytes   Unknown
                (Status.Modified,     _    ) -> returnBytes   Modified
                (Status.Affected,     _    ) -> returnBytes   Modified
                (Status.NonCacheable, _    ) -> returnNothing NonCacheable


reportIfVisible :: CallPointPath -> Session mm ()
reportIfVisible callPointPath = do
    Env.whenVisible callPointPath $
        getVarName callPointPath >>= report callPointPath


report :: CallPointPath -> VarName -> Session mm ()
report callPointPath varName = do
    resultCB  <- Env.getResultCallBack
    projectID <- Env.getProjectID
    time      <- Env.getTimeVar
    results   <- get varName time callPointPath
    safeLiftIO' (Error.CallbackError $(loc)) $
        resultCB projectID callPointPath results time


get :: VarName -> Time -> CallPointPath -> Session mm [ModeValue]
get varName time callPointPath = do
    modes <- Env.getSerializationModes callPointPath
    if MultiSet.null modes
        then logger debug "No serialization modes set" >> return []
        else do
            cinfo <- Env.cachedLookup callPointPath <??&> Error.OtherError $(loc) "Internal error"
            let distinctModes = MultiSet.distinctElems modes
                valCache = cinfo ^. CacheInfo.values
            (modValues, valCache') <- foldM (computeLookupValue varName time) ([], valCache) distinctModes
            Env.cachedInsert callPointPath $ CacheInfo.values .~ valCache' $ cinfo
            return modValues


computeLookupValue :: VarName -> Time -> ([ModeValue], CompValueMap)
                   -> Mode -> Session mm ([ModeValue], CompValueMap)
computeLookupValue varName time (modValues, compValMap) mode = do
    logger trace $ "Cached values count: " ++ show (Map.size compValMap)
    case Map.lookup (varName, mode) compValMap of
        Nothing -> do logger debug "Computing value"
                      val <- computeValue varName time mode
                      let newMap = if null $ varName ^. VarName.hash
                            then compValMap
                            else Map.insert (varName, mode) val compValMap
                      return (ModeValue mode (Just val):modValues, compValMap) --newMap) --FIXME[PM] : temporarily disabled
        justVal -> do logger debug "Cached value"
                      return (ModeValue mode justVal:modValues, compValMap)


computeValue :: VarName -> Time -> Mode -> Session mm SValue
computeValue varName time mode = do
    lift2 $ flip Catch.catch excHandler $ do
        let toValueExpr = "\\m -> flip computeValue m =<< toIOEnv (fromValue (" <> VarName.toString varName <> " (" <> show time <> ")))"
        logger trace toValueExpr
        action <- HEval.interpret'' toValueExpr "Mode -> IO (Maybe SValue)"
        liftIO $ action mode <??&.> "Internal error"
    where
        excHandler :: Catch.SomeException -> MGHC.Ghc SValue
        excHandler exc = case Catch.fromException exc of
            Just AbortException -> throw AbortException
            Nothing -> do
                logger L.error $ show exc
                liftIO (Serialization.toValue (ValueError.Error $ show exc) def) <??&.> "Internal error"


getVarName :: CallPointPath -> Session mm VarName
getVarName callPointPath = do
    let callPointLast = last callPointPath
        callPointInit = init callPointPath
    mfoldTop <- Flags.getFoldTop <$> Env.getFlags callPointLast
    let newCallPointPath = case mfoldTop of
            Nothing     -> callPointPath
            Just nodeID -> callPointInit
                        ++ [callPointLast & CallPoint.nodeID .~ nodeID]
    cacheInfo <- Cache.getCacheInfo newCallPointPath
    return $ cacheInfo ^. CacheInfo.recentVarName
