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

import           Flowbox.Interpreter.Session.Env     (Env)
import qualified Flowbox.Interpreter.Session.Env     as Env
import           Flowbox.Interpreter.Session.Error   (Error)
import qualified Flowbox.Interpreter.Session.Error   as Error
import qualified Flowbox.Interpreter.Session.Helpers as Helpers
import           Flowbox.Luna.Lib.LibManager         (LibManager)
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
    setHardodedExtensions
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


setHardodedExtensions :: Session ()
setHardodedExtensions = do
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
setLibManager libManager = lift $ modify (Env.libManager %~ const libManager)

getLibManager :: Session LibManager
getLibManager = gets $ view Env.libManager
