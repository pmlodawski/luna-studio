---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
module Test.Luna.Interpreter.Common where

import qualified Flowbox.Batch.Project.Project                                     as Project
import           Flowbox.Config.Config                                             (Config)
import qualified Flowbox.Config.Config                                             as Config
import           Flowbox.Control.Error
import           Flowbox.Data.Version                                              ()
import           Flowbox.Prelude
import qualified Flowbox.System.UniPath                                            as UniPath
import qualified Luna.DEP.AST.Control.Crumb                                        as Crumb
import qualified Luna.DEP.AST.Name                                                 as Name
import           Luna.DEP.Data.Source                                              (Source (Source))
import           Luna.DEP.Lib.Lib                                                  (Library (Library))
import qualified Luna.DEP.Lib.Lib                                                  as Library
import           Luna.DEP.Lib.Manager                                              (LibManager)
import qualified Luna.DEP.Lib.Manager                                              as LibManager
import qualified Luna.DEP.Pass.Analysis.Alias.Alias                                as Analysis.Alias
import qualified Luna.DEP.Pass.Analysis.CallGraph.CallGraph                        as Analysis.CallGraph
import qualified Luna.DEP.Pass.Transform.AST.DepSort.DepSort                       as Transform.DepSort
import qualified Luna.DEP.Pass.Transform.AST.Desugar.ImplicitCalls.ImplicitCalls   as Desugar.ImplicitCalls
import qualified Luna.DEP.Pass.Transform.AST.Desugar.ImplicitScopes.ImplicitScopes as Desugar.ImplicitScopes
import qualified Luna.DEP.Pass.Transform.AST.Desugar.ImplicitSelf.ImplicitSelf     as Desugar.ImplicitSelf
import qualified Luna.DEP.Pass.Transform.AST.Desugar.TLRecUpdt.TLRecUpdt           as Desugar.TLRecUpdt
import qualified Luna.DEP.Pass.Transform.AST.TxtParser.TxtParser                   as TxtParser
import           Luna.Interpreter.Session.Data.DefPoint                            (DefPoint (DefPoint))
import           Luna.Interpreter.Session.Env                                      (Env)
import qualified Luna.Interpreter.Session.Env                                      as Env
import qualified Luna.Interpreter.Session.Error                                    as Error
import           Luna.Interpreter.Session.Session                                  (Session)
import qualified Luna.Interpreter.Session.Session                                  as Session
import qualified Luna.Interpreter.Session.TargetHS.Reload                          as Reload



readCode :: String -> IO (LibManager, Library.ID)
readCode code = eitherStringToM' $ runEitherT $ do
    (ast, _, astInfo) <- EitherT $ TxtParser.run $ Source ["Main"] code
    (ast, astInfo)    <- EitherT $ Desugar.ImplicitSelf.run astInfo ast
    (ast, astInfo)    <- EitherT $ Desugar.TLRecUpdt.run astInfo ast
    aliasInfo         <- EitherT $ Analysis.Alias.run ast
    callGraph         <- EitherT $ Analysis.CallGraph.run aliasInfo ast
    ast               <- EitherT $ Transform.DepSort.run callGraph aliasInfo ast
    (ast, astInfo)    <- EitherT $ Desugar.ImplicitScopes.run astInfo aliasInfo ast
    (ast, astInfo)   <- EitherT $ Desugar.ImplicitCalls.run astInfo ast
    _aliasInfo        <- EitherT $ Analysis.Alias.run ast
    let path = UniPath.fromUnixString "."
    return $ LibManager.insNewNode (Library "Main" def path ast def astInfo) def


mkEnv :: Config -> mm -> String -> IO (Env mm, Library.ID)
mkEnv config mm code = do
    (libManager, libID) <- readCode code
    --putStrLn $ ppShow libManager
    let defPoint = (DefPoint libID [Crumb.Module "Main", Crumb.Function (Name.single "main") []])
    env <- Env.mk config mm libManager (Just $ Project.ID 0) (Just defPoint) $ const $ const $ const (void . return)-- curry print
    return (env, libID)


runSession :: mm -> String -> Session mm () -> IO ()
runSession mm code session = do
    cfg <- Config.load
    (env, libID) <- mkEnv cfg mm code

    result <- Session.run cfg env [] (Env.addReload libID Reload.ReloadLibrary >> session)
    eitherStringToM $ fmapL Error.format result
