---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Flowbox Team <contact@flowbox.io>, 2014
-- Proprietary and confidential
-- Unauthorized copying of this file, via any medium is strictly prohibited
---------------------------------------------------------------------------
module Luna.Interpreter.Session.Session where

import           Control.Monad.State
import           Control.Monad.Trans.Either
import           Data.ByteString.Lazy                (ByteString)
import qualified DynFlags                            as GHC
import qualified GHC
import qualified Language.Haskell.Interpreter        as I
import qualified Language.Haskell.Interpreter.Unsafe as I

import qualified Flowbox.Batch.Project.Project               as Project
import           Flowbox.Config.Config                       (Config)
import qualified Flowbox.Config.Config                       as Config
import           Flowbox.Control.Error
import           Flowbox.Prelude
import           Flowbox.System.Log.Logger
import qualified Luna.AST.Common                             as AST
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
import           Luna.Lib.Lib                                (Library)
import qualified Luna.Lib.Lib                                as Library
import           Luna.Lib.Manager                            (LibManager)
import qualified Luna.Lib.Manager                            as LibManager
import qualified Luna.Pass.Analysis.Alias.Alias              as Alias
import qualified Luna.Pass.Transform.Graph.Builder.Builder   as GraphBuilder



logger :: LoggerIO
logger = getLoggerIO "Luna.Interpreter.Session.Session"


type Session a = EitherT Error.ErrorStr (StateT Env I.Interpreter) a


run :: Config -> Env -> Session a -> IO (Either Error a)
run config env session = do
    result <- I.unsafeRunInterpreterWithTopDirAndArgs
                (Just $ Config.topDir $ Config.ghcS config)
                [ "-no-user-package-db"
                , "-package-db " ++ Config.pkgDb (Config.global config)
                , "-package-db " ++ Config.pkgDb (Config.local config)
                ]
               $ fst <$> runStateT (runEitherT (initialize >> session)) env
    return $ case result of
        Left e    -> Left $ Error.InterpreterError e
        Right res -> case res of
            Left e  -> Left $ Error.OtherError e
            Right r -> Right r


initialize :: Session ()
initialize = do
    lift2 $ I.reset
    flags <- lift2 $ I.runGhc GHC.getSessionDynFlags
    print $ length $ GHC.extensions flags

    setHardcodedExtensions
    flags <- lift2 $ I.runGhc GHC.getSessionDynFlags
    print $ length $ GHC.extensions flags

    lift2 $ I.setImportsQ [("Luna.Target.HS", Nothing)]
    runDecls Helpers.operation
    runDecls Helpers.hash


setImports :: [String] -> Session ()
setImports imports = lift2 $ I.runGhc $
    GHC.setContext $ map (GHC.IIDecl . GHC.simpleImportDecl . GHC.mkModuleName) imports



setFlags :: [GHC.ExtensionFlag] -> Session ()
setFlags flags = lift2 $ I.runGhc $ do
    current <- GHC.getSessionDynFlags
    void $ GHC.setSessionDynFlags $ foldl GHC.xopt_set current flags


unsetFlags :: [GHC.ExtensionFlag] -> Session ()
unsetFlags flags = lift2 $ I.runGhc $ do
    current <- GHC.getSessionDynFlags
    void $ GHC.setSessionDynFlags $ foldl GHC.xopt_unset current flags


runStmt :: String -> Session ()
runStmt stmt = do
    logger trace stmt
    result <- lift2 $ I.runGhc $ GHC.runStmt stmt GHC.RunToCompletion
    case result of
        GHC.RunOk _         -> return ()
        GHC.RunException ex -> left $ show ex
        GHC.RunBreak {}     -> left "Run break"


runDecls :: String -> Session ()
runDecls decls = do
    logger trace decls
    void $ lift2 $ I.runGhc $ GHC.runDecls decls


runAssignment :: String -> String -> Session ()
runAssignment asigned asignee =
    runStmt $ asigned ++ " <- return " ++ asignee


setHardcodedExtensions :: Session ()
setHardcodedExtensions = do
    setFlags   [ GHC.Opt_ImplicitPrelude
               , GHC.Opt_MonomorphismRestriction
               , GHC.Opt_DatatypeContexts
               , GHC.Opt_TraditionalRecordSyntax
               , GHC.Opt_EmptyDataDecls
               , GHC.Opt_ForeignFunctionInterface
               , GHC.Opt_PatternGuards
               , GHC.Opt_DoAndIfThenElse
               , GHC.Opt_RelaxedPolyRec
               , GHC.Opt_ExtendedDefaultRules
               ]

    setFlags   [ GHC.Opt_DataKinds
               , GHC.Opt_DeriveDataTypeable
               , GHC.Opt_DeriveGeneric
               , GHC.Opt_DysfunctionalDependencies
               , GHC.Opt_FlexibleContexts
               , GHC.Opt_FlexibleInstances
               , GHC.Opt_GADTs
               , GHC.Opt_RebindableSyntax
               , GHC.Opt_TemplateHaskell
               , GHC.Opt_UndecidableInstances

               , GHC.Opt_MultiParamTypeClasses
               , GHC.Opt_OverlappingInstances
               ]
    unsetFlags [ GHC.Opt_MonomorphismRestriction
               ]


setLibManager :: LibManager -> Session ()
setLibManager libManager = modify (Env.libManager .~ libManager)


getLibManager :: Session LibManager
getLibManager = gets $ view Env.libManager


getLibrary :: Library.ID -> Session Library
getLibrary libraryID = do
    libManager <- getLibManager
    LibManager.lab libManager libraryID <??> "Session.getLibrary : Cannot find library with id=" ++ show libraryID


getModule :: DefPoint -> Session Module
getModule (DefPoint libraryID bc) = do
    ast <- view Library.ast <$> getLibrary libraryID
    focus <- hoistEither $ Zipper.getFocus <$> Zipper.focusBreadcrumbs' bc ast
    Focus.getModule focus <??> "Session.getModule : Target is not a module"


getFunction :: DefPoint -> Session Expr
getFunction (DefPoint libraryID bc) = do
    ast <- view Library.ast <$> getLibrary libraryID
    focus <- hoistEither $ Zipper.getFocus <$> Zipper.focusBreadcrumbs' bc ast
    Focus.getFunction focus <??> "Session.getFunction : Target is not a function"


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
getMainPtr = gets $ view Env.mainPtr


getProjectID :: Session Project.ID
getProjectID = gets $ view Env.projectID


getResultCallBack :: Session (CallPointPath -> ByteString -> IO ())
getResultCallBack = gets $ view Env.resultCallBack
