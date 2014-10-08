---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Flowbox Team <contact@flowbox.io>, 2014
-- Proprietary and confidential
-- Unauthorized copying of this file, via any medium is strictly prohibited
---------------------------------------------------------------------------
{-# LANGUAGE TemplateHaskell #-}

module Luna.Interpreter.Session.Session where

import qualified Control.Monad.Ghc          as MGHC
import           Control.Monad.State
import           Control.Monad.Trans.Either
import qualified Data.Either                as Either
import qualified Data.Map                   as Map
import qualified Data.Maybe                 as Maybe
import           Data.Monoid                ((<>))
import           Data.Typeable              (Typeable)
import qualified DynFlags                   as GHC
import qualified GHC

import qualified Flowbox.Batch.Project.Project               as Project
import           Flowbox.Config.Config                       (Config)
import qualified Flowbox.Config.Config                       as Config
import           Flowbox.Control.Error
import           Flowbox.Prelude
import           Flowbox.Source.Location                     (Location, loc)
import           Flowbox.System.Log.Logger                   as Logger
import           Generated.Proto.Data.Value                  (Value)
import qualified Luna.AST.Common                             as AST
import           Luna.AST.Control.Focus                      (Focus)
import qualified Luna.AST.Control.Focus                      as Focus
import qualified Luna.AST.Control.Zipper                     as Zipper
import           Luna.AST.Expr                               (Expr)
import qualified Luna.AST.Expr                               as Expr
import           Luna.AST.Module                             (Module)
import           Luna.Graph.Graph                            (Graph)
import           Luna.Interpreter.Session.Data.CallPointPath (CallPointPath)
import           Luna.Interpreter.Session.Data.DefPoint      (DefPoint (DefPoint))
import qualified Luna.Interpreter.Session.Data.DefPoint      as DefPoint
import           Luna.Interpreter.Session.Env                (Env)
import qualified Luna.Interpreter.Session.Env                as Env
import           Luna.Interpreter.Session.Error              (Error)
import qualified Luna.Interpreter.Session.Error              as Error
import qualified Luna.Interpreter.Session.Helpers            as Helpers
import qualified Luna.Interpreter.Session.Hint.Eval          as HEval
import           Luna.Interpreter.Session.TargetHS.Reload    (Reload, ReloadMap)
import           Luna.Lib.Lib                                (Library)
import qualified Luna.Lib.Lib                                as Library
import           Luna.Lib.Manager                            (LibManager)
import qualified Luna.Lib.Manager                            as LibManager
import qualified Luna.Pass.Analysis.Alias.Alias              as Alias
import qualified Luna.Pass.Transform.Graph.Builder.Builder   as GraphBuilder



logger :: LoggerIO
logger = getLoggerIO "Luna.Interpreter.Session.Session"


type SessionST = StateT Env MGHC.Ghc


type Session = EitherT Error SessionST


type Import = String


run :: Config -> Env -> [Import] -> Session a -> IO (Either Error a)
run config env imports session =
    MGHC.runGhc (Just $ Config.topDir $ Config.ghcS config) $
        evalStateT (runEitherT (initialize config imports >> session)) env


--reifySession :: (GHC.Session -> Env -> IO a) -> Session a
--reifySession f = lift2 . f' =<< get where
--    --f' :: Env -> GHC.Ghc a
--    f' env' = GHC.reifyGhc (f'' env')
--    --f'' :: Env -> GHC.Session -> IO a
--    f'' = flip f


--reflectSession :: GHC.Session -> Env -> Session a -> IO a


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
    result <- interceptSourceErrors $ GHC.runStmtWithLocation location 1 stmt GHC.RunToCompletion
    case result of
        GHC.RunOk _         -> return ()
        GHC.RunException ex -> left $ Error.GhcRunError $(loc) ex
        GHC.RunBreak {}     -> left $ Error.OtherError  $(loc) "Run break"


runDecls :: String -> Session ()
runDecls decls = do
    logger trace decls
    void $ interceptSourceErrors $ GHC.runDeclsWithLocation location 1 decls


runAssignment :: String -> String -> Session ()
runAssignment asigned asignee =
    runDecls $ asigned ++ " = " ++ asignee


interpret :: Typeable a => String -> Session a
interpret = interceptSourceErrors . HEval.interpret


setHardcodedExtensions :: Session ()
setHardcodedExtensions =
    setFlags [ GHC.Opt_DataKinds ]


setAllReady :: Bool -> Session ()
setAllReady flag = modify $ Env.allReady .~ flag


getAllReady :: Session Bool
getAllReady = gets $ view Env.allReady


setLibManager :: LibManager -> Session ()
setLibManager libManager = modify $ Env.libManager .~ libManager


getLibManager :: Session LibManager
getLibManager = gets $ view Env.libManager


getLibrary :: Library.ID -> Session Library
getLibrary libraryID = do
    libManager <- getLibManager
    LibManager.lab libManager libraryID
        <??> Error.ASTLookupError $(loc) ("Cannot find library with id=" ++ show libraryID)


getModule :: DefPoint -> Session Module
getModule defPoint = do
    focus <- getFocus defPoint
    Focus.getModule focus
        <??> Error.ASTLookupError $(loc) "Target is not a module"


getFunction :: DefPoint -> Session Expr
getFunction defPoint = do
    focus <- getFocus defPoint
    Focus.getFunction focus
        <??> Error.ASTLookupError $(loc) "Target is not a function"


getClass :: DefPoint -> Session Expr
getClass defPoint = do
    focus <- getFocus defPoint
    Focus.getClass focus
        <??> Error.ASTLookupError $(loc) "Target is not a class"


getFocus :: DefPoint -> Session Focus
getFocus (DefPoint libraryID bc) = do
    ast <- view Library.ast <$> getLibrary libraryID
    hoistEither $ fmapL (Error.ASTLookupError $(loc)) $ Zipper.getFocus <$> Zipper.focusBreadcrumbs' bc ast


runPass :: Functor m => Location -> m (Either Error.ErrorStr a) -> EitherT Error m a
runPass loc' p = EitherT $ (fmap . fmapL) (Error.PassError loc') p


getGraph :: DefPoint -> Session (Graph, AST.ID)
getGraph defPoint = do
    library     <- getLibrary $ defPoint ^. DefPoint.libraryID
    let propertyMap = library ^. Library.propertyMap
        ast         = library ^. Library.ast
    expr  <- getFunction defPoint
    aa    <- runPass $(loc) $ Alias.run ast
    graph <- fst <$> runPass $(loc) (GraphBuilder.run aa propertyMap False expr)
    return (graph, expr ^. Expr.id)


getMainPtr :: Session DefPoint
getMainPtr = getMainPtrMaybe <??&> Error.ConfigError $(loc) "MainPtr not set."


getMainPtrMaybe :: Session (Maybe DefPoint)
getMainPtrMaybe = gets (view Env.mainPtr)


setMainPtr :: DefPoint -> Session ()
setMainPtr mainPtr = modify (Env.mainPtr .~ Just mainPtr)


getProjectID :: Session Project.ID
getProjectID = getProjectIDMaybe <??&> Error.ConfigError $(loc) "Project ID not set."


getProjectIDMaybe :: Session (Maybe Project.ID)
getProjectIDMaybe = gets (view Env.projectID)


setProjectID :: Project.ID -> Session ()
setProjectID projectID = modify (Env.projectID .~ Just projectID)


getResultCallBack :: Session (Project.ID -> CallPointPath -> Maybe Value -> IO ())
getResultCallBack = gets $ view Env.resultCallBack


addReload :: Library.ID -> Reload -> Session ()
addReload libraryID reload = modify (Env.reloadMap %~ update) where
    update = Map.alter (Just . (<> reload) . Maybe.fromMaybe def) libraryID


getReloads :: Session ReloadMap
getReloads = gets $ view Env.reloadMap


cleanReloads :: Session ()
cleanReloads = modify (Env.reloadMap .~ mempty)
