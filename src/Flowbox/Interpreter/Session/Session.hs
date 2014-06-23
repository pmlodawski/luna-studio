---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Flowbox Team <contact@flowbox.io>, 2014
-- Proprietary and confidential
-- Unauthorized copying of this file, via any medium is strictly prohibited
---------------------------------------------------------------------------
module Flowbox.Interpreter.Session.Session
( module I
, Session
, run
, setFlags
, unsetFlags
, setHardodedExtensions
) where

import qualified DynFlags                     as F
import qualified GHC
import           Language.Haskell.Interpreter as I

import qualified Flowbox.Interpreter.Session.Helpers as Helpers
import           Flowbox.Prelude
import           Flowbox.System.Log.Logger


logger :: LoggerIO
logger = getLoggerIO "Flowbox.Interpreter.Session.Session"


type Session a = I.Interpreter a


run :: Session a -> IO (Either I.InterpreterError a)
run session = I.runInterpreter $ initialize >> session


initialize :: Session ()
initialize = do
    I.reset
    setHardodedExtensions
    I.setImportsQ [("Prelude", Nothing)]
    void $ I.runGhc $ GHC.runDecls Helpers.helpers


setFlags :: [F.ExtensionFlag] -> Session ()
setFlags flags = I.runGhc $ do
    current <- GHC.getSessionDynFlags
    void $ GHC.setSessionDynFlags $ foldl F.xopt_set current flags


unsetFlags :: [F.ExtensionFlag] -> Session ()
unsetFlags flags = I.runGhc $ do
    current <- GHC.getSessionDynFlags
    void $ GHC.setSessionDynFlags $ foldl F.xopt_unset current flags


setHardodedExtensions :: Session ()
setHardodedExtensions = do
    setFlags [ F.Opt_MultiParamTypeClasses,
               F.Opt_FunctionalDependencies,
               F.Opt_FlexibleContexts,
               F.Opt_FlexibleInstances,
               F.Opt_GADTs,
               F.Opt_OverlappingInstances,
               F.Opt_UndecidableInstances]
    unsetFlags []
