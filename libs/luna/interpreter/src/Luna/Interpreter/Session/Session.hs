---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Flowbox Team <contact@flowbox.io>, 2014
-- Proprietary and confidential
-- Unauthorized copying of this file, via any medium is strictly prohibited
---------------------------------------------------------------------------
{-# LANGUAGE TemplateHaskell #-}

module Luna.Interpreter.Session.Session (
    Session,
    SessionST,
    module Luna.Interpreter.Session.Session,
) where

import           Control.Monad.Catch                        (bracket)
import qualified Control.Monad.Catch                        as Catch
import qualified Control.Monad.Ghc                          as MGHC
import           Control.Monad.State
import           Control.Monad.Trans.Either
import qualified Data.Either                                as Either
import qualified DynFlags                                   as GHC
import qualified GHC
import qualified HscTypes

import           Flowbox.Config.Config                      (Config)
import qualified Flowbox.Config.Config                      as Config
import           Flowbox.Prelude
import           Flowbox.Source.Location                    (Location, loc)
import           Flowbox.System.FilePath                    (expand')
import           Flowbox.System.Log.Logger                  as Logger
import           Luna.Interpreter.Session.Env               (Env, Session, SessionST)
import qualified Luna.Interpreter.Session.Env               as Env
import           Luna.Interpreter.Session.Error             (Error)
import qualified Luna.Interpreter.Session.Error             as Error
import qualified Luna.Interpreter.Session.Hint.Eval         as HEval
import qualified Luna.Interpreter.Session.TargetHS.Bindings as Bindings



logger :: LoggerIO
logger = getLoggerIO $moduleName


type Import = String


run :: Config -> Env mm -> [Import] -> Session mm a -> IO (Either Error a)
run config env imports session = do
    topDir <- liftIO $ expand' $ Config.topDir $ Config.ghcS config
    MGHC.runGhc (Just topDir) $
        evalStateT (runEitherT (initialize config imports >> session)) env


initialize :: Config -> [Import] -> Session mm ()
initialize config imports = do
    globalPkgDb <- liftIO $ expand' $ Config.pkgDb $ Config.global config
    localPkgDb  <- liftIO $ expand' $ Config.pkgDb $ Config.local config
    setStrFlags ["-fno-ghci-sandbox"]
    let isNotUser GHC.UserPkgConf = False
        isNotUser _ = True
        extraPkgConfs p = [ GHC.PkgConfFile globalPkgDb
                          , GHC.PkgConfFile localPkgDb
                          ] ++ filter isNotUser p
    flags <- lift2   GHC.getSessionDynFlags
    _     <- lift2 $ GHC.setSessionDynFlags flags
                   { GHC.extraPkgConfs = extraPkgConfs
                   , GHC.hscTarget     = GHC.HscInterpreted
                   , GHC.ghcLink       = GHC.LinkInMemory
                   , GHC.ctxtStkDepth  = 1000
                   --, GHC.verbosity = 4
                   }
    setHardcodedExtensions
    setImports $ "Luna.Target.HS"
               : "Luna.Interpreter.Runtime"
               : imports
    logger info "Initialization done"


setStrFlags :: [String] -> Session mm ()
setStrFlags strFlags = do
    flags <- lift2 GHC.getInteractiveDynFlags
    (flags2, leftovers, warns) <- lift2 $ GHC.parseDynamicFlags flags $ map GHC.noLoc strFlags
    liftIO $ HscTypes.handleFlagWarnings flags2 warns
    let unrecognized = map (show . GHC.unLoc) leftovers
    unless (null unrecognized) $
        left $ Error.OtherError $(loc) $ "Unrecognized flags: " ++ unwords unrecognized
    void $ lift2 $ GHC.setInteractiveDynFlags flags2


setImports :: [Import] -> Session mm ()
setImports = lift2 . GHC.setContext . map (GHC.IIDecl . GHC.simpleImportDecl . GHC.mkModuleName)


withImports :: [Import] -> Session mm a -> Session mm a
withImports imports action = sandboxContext $ do
    setImports imports
    action


setFlags :: [GHC.ExtensionFlag] -> Session mm ()
setFlags flags = lift2 $ do
    current <- GHC.getSessionDynFlags
    void $ GHC.setSessionDynFlags $ foldl GHC.xopt_set current flags


unsetFlags :: [GHC.ExtensionFlag] -> Session mm ()
unsetFlags flags = lift2 $ do
    current <- GHC.getSessionDynFlags
    void $ GHC.setSessionDynFlags $ foldl GHC.xopt_unset current flags


withExtensionFlags :: [GHC.ExtensionFlag] -> [GHC.ExtensionFlag] -> Session mm a -> Session mm a
withExtensionFlags enable disable action = sandboxDynFlags $ do
    setFlags enable
    unsetFlags disable
    action


sandboxDynFlags :: Session mm a -> Session mm a
sandboxDynFlags action =
    hoistEither =<< lift (bracket (lift   GHC.getSessionDynFlags)
                                  (lift . GHC.setSessionDynFlags)
                                  (const $ runEitherT action))


sandboxContext :: Session mm a -> Session mm a
sandboxContext action =
    hoistEither =<< lift (bracket (lift   GHC.getContext)
                                  (lift . GHC.setContext)
                                  (const $ runEitherT action))


location :: String
location = "<target ghc-hs interactive>"


interceptSourceErrors :: MGHC.Ghc a -> Session mm a
interceptSourceErrors ghc = do
    let handler srcErr = do
            let errDat = Error.SourceError $(loc) srcErr
                errMsg = Error.format errDat
            logger Logger.error errMsg
            return $ Left errDat
    r <- lift2 $ GHC.handleSourceError handler $ Right <$> ghc
    hoistEither r


interceptErrors :: Location -> MGHC.Ghc a -> Session mm a
interceptErrors loc' ghc = do
    sessionBackup <- lift2 GHC.getSession
    let handler :: Catch.SomeException -> MGHC.Ghc (Either Error a)
        handler otherErr = do
            let errDat = Error.OtherError loc' $ show otherErr
                errMsg = Error.format errDat
            logger Logger.error errMsg
            GHC.setSession sessionBackup
            return $ Left errDat
    r <- lift2 $ Catch.catch (Right <$> ghc) handler
    hoistEither r


atomically :: Session mm a -> Session mm a
atomically f = do
    sessionBackup <- lift2 GHC.getSession
    result <- lift $ runEitherT f
    when (Either.isLeft result) $
        lift2 $ GHC.setSession sessionBackup
    hoistEither result


runStmt :: String -> Session mm ()
runStmt stmt = do
    logger trace stmt
    result <- interceptErrors $(loc) $ GHC.runStmtWithLocation location 1 stmt GHC.RunToCompletion
    case result of
        GHC.RunOk _         -> return ()
        GHC.RunException ex -> left $ Error.GhcRunError $(loc) ex
        GHC.RunBreak {}     -> left $ Error.OtherError  $(loc) "Run break"


runDecls :: String -> Session mm ()
runDecls decls = do
    logger trace decls
    void $ interceptErrors $(loc) $ GHC.runDeclsWithLocation location 1 decls


runAssignment :: String -> String -> Session mm ()
runAssignment asigned asignee = Env.fragile $ do
    lift2 $ Bindings.remove asigned
    -- do not use runDecls here: its bindings are hard to remove and cause memory leaks!
    runStmt $ "let " <> asigned <> " = " <> asignee


runAssignment' :: String -> String -> Session mm ()
runAssignment' asigned asignee = do
    lift2 $ Bindings.remove asigned
    runStmt $ asigned ++ " <- " ++ asignee


interpret :: Typeable a => String -> Session mm a
interpret = interceptErrors $(loc) . HEval.interpret


setHardcodedExtensions :: Session mm ()
setHardcodedExtensions =
    setFlags [ GHC.Opt_DataKinds
             , GHC.Opt_ScopedTypeVariables
             ]


--reifySession :: (GHC.Session -> Env -> IO a) -> Session mm a
--reifySession f = lift2 . f' =<< get where
--    --f' :: Env -> GHC.Ghc a
--    f' env' = GHC.reifyGhc (f'' env')
--    --f'' :: Env -> GHC.Session -> IO a
--    f'' = flip f


--reflectSession :: GHC.Session -> Env -> Session mm a -> IO a
