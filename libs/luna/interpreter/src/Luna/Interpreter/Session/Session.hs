---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Flowbox Team <contact@flowbox.io>, 2014
-- Proprietary and confidential
-- Unauthorized copying of this file, via any medium is strictly prohibited
---------------------------------------------------------------------------
module Luna.Interpreter.Session.Session where

import           Control.Monad.State
import           Control.Monad.Trans.Either
import qualified DynFlags                     as F
import qualified GHC
import qualified Language.Haskell.Interpreter as I

import qualified Flowbox.Batch.Project.Project             as Project
import           Flowbox.Control.Error
import           Flowbox.Prelude
import           Flowbox.System.Log.Logger
import qualified Luna.AST.Common                           as AST
import qualified Luna.AST.Control.Focus                    as Focus
import qualified Luna.AST.Control.Zipper                   as Zipper
import           Luna.AST.Expr                             (Expr)
import qualified Luna.AST.Expr                             as Expr
import           Luna.Graph.Graph                          (Graph)
import           Luna.Interpreter.Session.Data.DefPoint    (DefPoint (DefPoint))
import qualified Luna.Interpreter.Session.Data.DefPoint    as DefPoint
import           Luna.Interpreter.Session.Env              (Env)
import qualified Luna.Interpreter.Session.Env              as Env
import           Luna.Interpreter.Session.Error            (Error)
import qualified Luna.Interpreter.Session.Error            as Error
import qualified Luna.Interpreter.Session.Helpers          as Helpers
import           Luna.Lib.Lib                              (Library)
import qualified Luna.Lib.Lib                              as Library
import           Luna.Lib.Manager                          (LibManager)
import qualified Luna.Lib.Manager                          as LibManager
import qualified Luna.Pass.Analysis.Alias.Alias            as Alias
import qualified Luna.Pass.Transform.Graph.Builder.Builder as GraphBuilder



logger :: LoggerIO
logger = getLoggerIO "Luna.Interpreter.Session.Session"


type Session a = EitherT Error.ErrorStr (StateT Env I.Interpreter) a


run :: Env -> Session a -> IO (Either Error a)
run env session = do
    result <- I.runInterpreter
            $ fst <$> runStateT (runEitherT (initialize >> session)) env
    return $ case result of
        Left e    -> Left $ Error.InterpreterError e
        Right res -> case res of
            Left e  -> Left $ Error.OtherError e
            Right r -> Right r


initialize :: Session ()
initialize = do
    lift2 I.reset
    setHardcodedExtensions
    lift2 $ I.setImportsQ [("Prelude", Nothing)
                          ,("Control.Monad", Nothing)
                          ,("Data.Hash", Just "Data.Hash")
                          ,("Data.Word", Nothing)
                          ]
    runDecls Helpers.operation
    runDecls Helpers.hash


setFlags :: [F.ExtensionFlag] -> Session ()
setFlags flags = lift2 $ I.runGhc $ do
    current <- GHC.getSessionDynFlags
    void $ GHC.setSessionDynFlags $ foldl F.xopt_set current flags


unsetFlags :: [F.ExtensionFlag] -> Session ()
unsetFlags flags = lift2 $ I.runGhc $ do
    current <- GHC.getSessionDynFlags
    void $ GHC.setSessionDynFlags $ foldl F.xopt_unset current flags


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
    setFlags [ F.Opt_EmptyDataDecls
             , F.Opt_FlexibleContexts
             , F.Opt_FlexibleInstances
             , F.Opt_FunctionalDependencies
             , F.Opt_GADTs
             , F.Opt_MultiParamTypeClasses
             , F.Opt_OverlappingInstances
             , F.Opt_ScopedTypeVariables
             , F.Opt_UndecidableInstances
             --, F.Opt_IncoherentInstances
             ]
    unsetFlags []


setLibManager :: LibManager -> Session ()
setLibManager libManager = modify (Env.libManager .~ libManager)


getLibManager :: Session LibManager
getLibManager = gets $ view Env.libManager


getLibrary :: Library.ID -> Session Library
getLibrary libraryID = do
    libManager <- getLibManager
    LibManager.lab libManager libraryID <??> "Session.getLibrary : Cannot find library with id=" ++ show libraryID


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


findMain :: Session DefPoint
findMain = gets $ view Env.mainPtr


getProjectID :: Session Project.ID
getProjectID = gets $ view Env.projectID
