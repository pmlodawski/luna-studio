---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Flowbox Team <contact@flowbox.io>, 2014
-- Proprietary and confidential
-- Unauthorized copying of this file, via any medium is strictly prohibited
---------------------------------------------------------------------------
module Flowbox.Interpreter.Session.Session where

import           Control.Monad.State
import           Control.Monad.Trans.Either
import qualified DynFlags                     as F
import qualified GHC
import qualified Language.Haskell.Interpreter as I

import           Flowbox.Control.Error
import           Flowbox.Interpreter.Session.Data.DefPoint           (DefPoint (DefPoint))
import qualified Flowbox.Interpreter.Session.Data.DefPoint           as DefPoint
import           Flowbox.Interpreter.Session.Env                     (Env)
import qualified Flowbox.Interpreter.Session.Env                     as Env
import           Flowbox.Interpreter.Session.Error                   (Error)
import qualified Flowbox.Interpreter.Session.Error                   as Error
import qualified Flowbox.Interpreter.Session.Helpers                 as Helpers
import qualified Flowbox.Luna.Data.AST.Common                        as AST
import           Flowbox.Luna.Data.AST.Expr                          (Expr)
import qualified Flowbox.Luna.Data.AST.Expr                          as Expr
import qualified Flowbox.Luna.Data.AST.Zipper.Focus                  as Focus
import qualified Flowbox.Luna.Data.AST.Zipper.Zipper                 as Zipper
import           Flowbox.Luna.Data.Graph.Graph                       (Graph)
import           Flowbox.Luna.Lib.LibManager                         (LibManager)
import qualified Flowbox.Luna.Lib.LibManager                         as LibManager
import           Flowbox.Luna.Lib.Library                            (Library)
import qualified Flowbox.Luna.Lib.Library                            as Library
import qualified Flowbox.Luna.Passes.Analysis.Alias.Alias            as Alias
import qualified Flowbox.Luna.Passes.Transform.Graph.Builder.Builder as GraphBuilder
import           Flowbox.Prelude
import           Flowbox.System.Log.Logger



logger :: LoggerIO
logger = getLoggerIO "Flowbox.Interpreter.Session.Session"


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
                          ,("Control.Monad", Nothing)]
    runDecls Helpers.helpers


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


setHardcodedExtensions :: Session ()
setHardcodedExtensions = do
    setFlags [ F.Opt_MultiParamTypeClasses
             , F.Opt_FunctionalDependencies
             , F.Opt_FlexibleContexts
             , F.Opt_FlexibleInstances
             , F.Opt_GADTs
             , F.Opt_OverlappingInstances
             --, F.Opt_UndecidableInstances
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
    LibManager.lab libManager libraryID <??> "Cannot find library with id=" ++ show libraryID


getFunction :: DefPoint -> Session Expr
getFunction (DefPoint libraryID bc) = do
    ast <- view Library.ast <$> getLibrary libraryID
    focus <- hoistEither $ Zipper.getFocus <$> Zipper.focusBreadcrumbs' bc ast
    Focus.getFunction focus <??> "Target is not a function"


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
