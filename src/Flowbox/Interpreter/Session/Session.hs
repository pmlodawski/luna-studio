---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Flowbox Team <contact@flowbox.io>, 2014
-- Proprietary and confidential
-- Unauthorized copying of this file, via any medium is strictly prohibited
---------------------------------------------------------------------------
module Flowbox.Interpreter.Session.Session
( module Flowbox.Interpreter.Session.Session
, module I
) where

import           Control.Monad.Trans.State
import qualified DynFlags                     as F
import qualified GHC
import           Language.Haskell.Interpreter as I

import           Flowbox.Interpreter.Session.Env     (Env)
import qualified Flowbox.Interpreter.Session.Env     as Env
import qualified Flowbox.Interpreter.Session.Helpers as Helpers
import           Flowbox.Luna.Lib.LibManager         (LibManager)
import           Flowbox.Prelude
import           Flowbox.System.Log.Logger



logger :: LoggerIO
logger = getLoggerIO "Flowbox.Interpreter.Session.Session"



type Session a = StateT Env I.Interpreter a


run :: Env -> Session a -> IO (Either I.InterpreterError a)
run env session =
    I.runInterpreter $ fst <$> runStateT (initialize >> session) env


initialize :: Session ()
initialize = do
    lift I.reset
    setHardodedExtensions
    lift $ I.setImportsQ [("Prelude", Nothing)
                         ,("Control.Monad", Nothing)]
    runDecls Helpers.helpers


setFlags :: [F.ExtensionFlag] -> Session ()
setFlags flags = lift $ I.runGhc $ do
    current <- GHC.getSessionDynFlags
    void $ GHC.setSessionDynFlags $ foldl F.xopt_set current flags


unsetFlags :: [F.ExtensionFlag] -> Session ()
unsetFlags flags = lift $ I.runGhc $ do
    current <- GHC.getSessionDynFlags
    void $ GHC.setSessionDynFlags $ foldl F.xopt_unset current flags


runStmt :: String -> Session ()
runStmt stmt = do
    result <- lift $ I.runGhc $ GHC.runStmt stmt GHC.RunToCompletion
    case result of
        GHC.RunOk _         -> return ()
        GHC.RunException ex -> fail $ show ex
        GHC.RunBreak {}     -> fail "Run break"


runDecls :: String -> Session ()
runDecls decls = void $ lift $ I.runGhc $ GHC.runDecls decls


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
setLibManager libManager = modify (Env.libManager %~ const libManager)

getLibManager :: Session LibManager
getLibManager = gets (view Env.libManager)
