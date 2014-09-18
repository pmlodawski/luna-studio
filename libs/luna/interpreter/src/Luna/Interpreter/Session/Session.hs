---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Flowbox Team <contact@flowbox.io>, 2014
-- Proprietary and confidential
-- Unauthorized copying of this file, via any medium is strictly prohibited
---------------------------------------------------------------------------
module Luna.Interpreter.Session.Session where

import           Control.Monad.State
import           Control.Monad.Trans.Either
import           Data.ByteString.Lazy       (ByteString)
import  Data.Typeable (Typeable)
import qualified Data.Map                   as Map
import qualified Data.Maybe                 as Maybe
import           Data.Monoid                ((<>))
import qualified DynFlags                   as GHC
import qualified GHC

import qualified Flowbox.Batch.Project.Project               as Project
import           Flowbox.Config.Config                       (Config)
import qualified Flowbox.Config.Config                       as Config
import           Flowbox.Control.Error
import           Flowbox.Prelude
import           Flowbox.System.Log.Logger
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


type SessionST = StateT Env GHC.Ghc


type Session = EitherT Error.ErrorStr SessionST


type Import = String


run :: Config -> Env -> [Import] -> Session a -> IO (Either Error a)
run config env imports session = do
    result <- GHC.runGhc (Just $ Config.topDir $ Config.ghcS config)
    --I.unsafeRunInterpreterWithTopDirAndArgs
    --            (Just $ Config.topDir $ Config.ghcS config)
    --            [ "-no-user-package-db"
    --            , "-package-db " ++ Config.pkgDb (Config.global config)
    --            , "-package-db " ++ Config.pkgDb (Config.local config)
    --            ]
               $ fst <$> runStateT (runEitherT (initialize config imports >> session)) env
    return $ case result of
        Left e    -> Left $ Error.OtherError e
        Right res -> Right res


initialize :: Config -> [Import] -> Session ()
initialize config imports = do
    flags <- lift2 $ GHC.getSessionDynFlags
    _ <- lift2 $ GHC.setSessionDynFlags flags
                { GHC.extraPkgConfs = ( [ GHC.PkgConfFile $ Config.pkgDb $ Config.global config
                                        , GHC.PkgConfFile $ Config.pkgDb $ Config.local config
                                        ] ++) . GHC.extraPkgConfs flags
                , GHC.hscTarget = GHC.HscInterpreted
                , GHC.ghcLink   = GHC.LinkInMemory
                --, GHC.verbosity = 4
                }
    setHardcodedExtensions
    setImports ([ "Data.Word"
                , "Luna.Target.HS"
                ] ++ imports)
    runDecls Helpers.hash


setImports :: [Import] -> Session ()
setImports = lift2 . GHC.setContext . map (GHC.IIDecl . GHC.simpleImportDecl . GHC.mkModuleName)


setFlags :: [GHC.ExtensionFlag] -> Session ()
setFlags flags = lift2 $ do
    current <- GHC.getSessionDynFlags
    void $ GHC.setSessionDynFlags $ foldl GHC.xopt_set current flags


unsetFlags :: [GHC.ExtensionFlag] -> Session ()
unsetFlags flags = lift2 $ do
    current <- GHC.getSessionDynFlags
    void $ GHC.setSessionDynFlags $ foldl GHC.xopt_unset current flags


withFlags :: [GHC.ExtensionFlag] -> [GHC.ExtensionFlag] -> Session a -> Session a
withFlags enable disable action = do
    flags <- lift2 GHC.getSessionDynFlags
    setFlags enable
    unsetFlags disable
    result <- action
    _ <- lift2 $ GHC.setSessionDynFlags flags
    return result


location :: String
location = "<target ghc-hs interactive>"


runStmt :: String -> Session ()
runStmt stmt = do
    logger trace stmt
    result <- lift2 $ GHC.runStmtWithLocation location 1 stmt GHC.RunToCompletion
    case result of
        GHC.RunOk _         -> return ()
        GHC.RunException ex -> left $ show ex
        GHC.RunBreak {}     -> left "Run break"


runDecls :: String -> Session ()
runDecls decls = do
    logger trace decls
    void $ lift2 $ GHC.runDeclsWithLocation location 1 decls



runAssignment :: String -> String -> Session ()
runAssignment asigned asignee =
    runDecls $ asigned ++ " = " ++ asignee


interpret :: Typeable a => String -> Session a
interpret = lift2 . HEval.interpret


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
    LibManager.lab libManager libraryID <??> "Session.getLibrary : Cannot find library with id=" ++ show libraryID


getModule :: DefPoint -> Session Module
getModule defPoint = do
    focus <- getFocus defPoint
    Focus.getModule focus <??> "Session.getModule : Target is not a module"


getFunction :: DefPoint -> Session Expr
getFunction defPoint = do
    focus <- getFocus defPoint
    Focus.getFunction focus <??> "Session.getFunction : Target is not a function"


getClass :: DefPoint -> Session Expr
getClass defPoint = do
    focus <- getFocus defPoint
    Focus.getClass focus <??> "Session.getClass : Target is not a class"


getFocus :: DefPoint -> Session Focus
getFocus (DefPoint libraryID bc) = do
    ast <- view Library.ast <$> getLibrary libraryID
    hoistEither $ Zipper.getFocus <$> Zipper.focusBreadcrumbs' bc ast


getGraph :: DefPoint -> Session (Graph, AST.ID)
getGraph defPoint = do
    library     <- getLibrary $ defPoint ^. DefPoint.libraryID
    let propertyMap = library ^. Library.propertyMap
        ast         = library ^. Library.ast
    expr  <- getFunction defPoint
    aa    <- EitherT $ Alias.run ast
    graph <- fst <$> (EitherT $ GraphBuilder.run aa propertyMap expr)
    return (graph, expr ^. Expr.id)


getMainPtr :: Session DefPoint
getMainPtr = getMainPtrMaybe <??&> "MainPtr not set."


getMainPtrMaybe :: Session (Maybe DefPoint)
getMainPtrMaybe = gets (view Env.mainPtr)


setMainPtr :: DefPoint -> Session ()
setMainPtr mainPtr = modify (Env.mainPtr .~ Just mainPtr)


getProjectID :: Session Project.ID
getProjectID = getProjectIDMaybe <??&> "Project ID not set."


getProjectIDMaybe :: Session (Maybe Project.ID)
getProjectIDMaybe = gets (view Env.projectID)


setProjectID :: Project.ID -> Session ()
setProjectID projectID = modify (Env.projectID .~ Just projectID)


getResultCallBack :: Session (Project.ID -> CallPointPath -> ByteString -> IO ())
getResultCallBack = gets $ view Env.resultCallBack


addReload :: Library.ID -> Reload -> Session ()
addReload libraryID reload = modify (Env.reloadMap %~ update) where
    update = Map.alter (Just . (<> reload) . Maybe.fromMaybe def) libraryID


getReloads :: Session ReloadMap
getReloads = gets $ view Env.reloadMap


cleanReloads :: Session ()
cleanReloads = modify (Env.reloadMap .~ mempty)
