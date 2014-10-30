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

import qualified Control.Monad.Catch        as Catch
import qualified Control.Monad.Ghc          as MGHC
import           Control.Monad.State
import           Control.Monad.Trans.Either
import qualified Data.Either                as Either
import           Data.Typeable              (Typeable)
import qualified DynFlags                   as GHC
import qualified GHC

import           Flowbox.Config.Config              (Config)
import qualified Flowbox.Config.Config              as Config
import           Flowbox.Prelude
import           Flowbox.Source.Location            (loc)
import           Flowbox.System.Log.Logger          as Logger
import           Luna.Interpreter.Session.Env       (Env, Session, SessionST)
import           Luna.Interpreter.Session.Error     (Error)
import qualified Luna.Interpreter.Session.Error     as Error
import qualified Luna.Interpreter.Session.Helpers   as Helpers
import qualified Luna.Interpreter.Session.Hint.Eval as HEval



logger :: LoggerIO
logger = getLoggerIO $(moduleName)



type Import = String


run :: Config -> Env -> [Import] -> Session a -> IO (Either Error a)
run config env imports session =
    MGHC.runGhc (Just $ Config.topDir $ Config.ghcS config) $
        evalStateT (runEitherT (initialize config imports >> session)) env


initialize :: Config -> [Import] -> Session ()
initialize config imports = do
    let isNotUser GHC.UserPkgConf = False
        isNotUser _ = True
        extraPkgConfs p = [ GHC.PkgConfFile $ Config.pkgDb $ Config.global config
                          , GHC.PkgConfFile $ Config.pkgDb $ Config.local config
                          ] ++ filter isNotUser p
    flags <- lift2 GHC.getSessionDynFlags
    _  <- lift2 $ GHC.setSessionDynFlags flags
                { GHC.extraPkgConfs = extraPkgConfs
                , GHC.hscTarget = GHC.HscInterpreted
                , GHC.ghcLink   = GHC.LinkInMemory
                --, GHC.verbosity = 4
                }
    setHardcodedExtensions
    setImports $ "Data.Word"
               : "Luna.Target.HS"
               : "System.Mem"
               : imports
    runDecls Helpers.hash


setImports :: [Import] -> Session ()
setImports = lift2 . GHC.setContext . map (GHC.IIDecl . GHC.simpleImportDecl . GHC.mkModuleName)


withImports :: [Import] -> Session a -> Session a
withImports imports action = sandboxContext $ do
    setImports imports
    action


setFlags :: [GHC.ExtensionFlag] -> Session ()
setFlags flags = lift2 $ do
    current <- GHC.getSessionDynFlags
    void $ GHC.setSessionDynFlags $ foldl GHC.xopt_set current flags


unsetFlags :: [GHC.ExtensionFlag] -> Session ()
unsetFlags flags = lift2 $ do
    current <- GHC.getSessionDynFlags
    void $ GHC.setSessionDynFlags $ foldl GHC.xopt_unset current flags


withExtensionFlags :: [GHC.ExtensionFlag] -> [GHC.ExtensionFlag] -> Session a -> Session a
withExtensionFlags enable disable action = sandboxDynFlags $ do
    setFlags enable
    unsetFlags disable
    action


sandboxDynFlags :: Session a -> Session a
sandboxDynFlags action = do
    flags  <- lift2 GHC.getSessionDynFlags
    result <- action
    _ <- lift2 $ GHC.setSessionDynFlags flags
    return result


sandboxContext :: Session a -> Session a
sandboxContext action = do
    context <- lift2 GHC.getContext
    result  <- action
    lift2 $ GHC.setContext context
    return result


location :: String
location = "<target ghc-hs interactive>"


interceptSourceErrors :: MGHC.Ghc a -> Session a
interceptSourceErrors ghc = do
    let handler srcErr = do
            let errDat = Error.SourceError $(loc) srcErr
                errMsg = Error.format errDat
            logger Logger.error errMsg
            return $ Left errDat
    r <- lift2 $ GHC.handleSourceError handler $ Right <$> ghc
    hoistEither r


interceptErrors :: MGHC.Ghc a -> Session a
interceptErrors ghc = do
    let handler :: Catch.SomeException -> MGHC.Ghc (Either Error a)
        handler otherErr = do
            let errDat = Error.OtherError $(loc) $ show otherErr
                errMsg = Error.format errDat
            logger Logger.error errMsg
            return $ Left errDat
    r <- lift2 $ Catch.catch (Right <$> ghc) handler
    hoistEither r


atomically :: Session a -> Session a
atomically f = do
    sessionBackup <- lift2 GHC.getSession
    result <- lift $ runEitherT f
    when (Either.isLeft result) $
        lift2 $ GHC.setSession sessionBackup
    hoistEither result


runStmt :: String -> Session ()
runStmt stmt = do
    logger trace stmt
    result <- interceptErrors $ GHC.runStmtWithLocation location 1 stmt GHC.RunToCompletion
    case result of
        GHC.RunOk _         -> return ()
        GHC.RunException ex -> left $ Error.GhcRunError $(loc) ex
        GHC.RunBreak {}     -> left $ Error.OtherError  $(loc) "Run break"


runDecls :: String -> Session ()
runDecls decls = do
    logger trace decls
    void $ interceptErrors $ GHC.runDeclsWithLocation location 1 decls


runAssignment :: String -> String -> Session ()
runAssignment asigned asignee =
    runDecls $ asigned ++ " = " ++ asignee


interpret :: Typeable a => String -> Session a
interpret = interceptErrors . HEval.interpret


setHardcodedExtensions :: Session ()
setHardcodedExtensions =
    setFlags [ GHC.Opt_DataKinds ]


--reifySession :: (GHC.Session -> Env -> IO a) -> Session a
--reifySession f = lift2 . f' =<< get where
--    --f' :: Env -> GHC.Ghc a
--    f' env' = GHC.reifyGhc (f'' env')
--    --f'' :: Env -> GHC.Session -> IO a
--    f'' = flip f


--reflectSession :: GHC.Session -> Env -> Session a -> IO a
