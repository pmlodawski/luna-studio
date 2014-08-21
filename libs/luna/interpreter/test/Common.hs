---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
module Common where

import qualified Flowbox.Config.Config                                         as Config
import           Flowbox.Control.Error
import           Flowbox.Prelude
import qualified Flowbox.System.UniPath                                        as UniPath
import qualified Luna.AST.Control.Crumb                                        as Crumb
import           Luna.Data.Source                                              (Source (Source))
import qualified Luna.Graph.PropertyMap                                        as PropertyMap
import           Luna.Interpreter.Session.Data.DefPoint                        (DefPoint (DefPoint))
import           Luna.Interpreter.Session.Env                                  (Env)
import qualified Luna.Interpreter.Session.Env                                  as Env
import qualified Luna.Interpreter.Session.Error                                as Error
import           Luna.Interpreter.Session.Session                              (Session)
import qualified Luna.Interpreter.Session.Session                              as Session
import           Luna.Lib.Lib                                                  (Library (Library))
import qualified Luna.Lib.Lib                                                  as Library
import           Luna.Lib.Manager                                              (LibManager)
import qualified Luna.Lib.Manager                                              as LibManager
import qualified Luna.Pass.Analysis.Alias.Alias                                as Analysis.Alias
import qualified Luna.Pass.Analysis.CallGraph.CallGraph                        as Analysis.CallGraph
import qualified Luna.Pass.Transform.AST.DepSort.DepSort                       as Transform.DepSort
import qualified Luna.Pass.Transform.AST.Desugar.ImplicitCalls.ImplicitCalls   as Desugar.ImplicitCalls
import qualified Luna.Pass.Transform.AST.Desugar.ImplicitScopes.ImplicitScopes as Desugar.ImplicitScopes
import qualified Luna.Pass.Transform.AST.Desugar.ImplicitSelf.ImplicitSelf     as Desugar.ImplicitSelf
import qualified Luna.Pass.Transform.AST.Desugar.TLRecUpdt.TLRecUpdt           as Desugar.TLRecUpdt
import qualified Luna.Pass.Transform.AST.TxtParser.TxtParser                   as TxtParser



readCode :: String -> IO (LibManager, Library.ID)
readCode code = eitherStringToM' $ runEitherT $ do
    (ast, _, astInfo) <- EitherT $ TxtParser.run $ Source ["Main"] code
    (ast, astInfo)    <- EitherT $ Desugar.ImplicitSelf.run astInfo ast
    (ast, astInfo)    <- EitherT $ Desugar.TLRecUpdt.run astInfo ast
    aliasInfo         <- EitherT $ Analysis.Alias.run ast
    callGraph         <- EitherT $ Analysis.CallGraph.run aliasInfo ast
    ast               <- EitherT $ Transform.DepSort.run callGraph aliasInfo ast
    (ast, astInfo)    <- EitherT $ Desugar.ImplicitScopes.run astInfo aliasInfo ast
    (ast, _astInfo)   <- EitherT $ Desugar.ImplicitCalls.run astInfo ast
    _aliasInfo        <- EitherT $ Analysis.Alias.run ast
    let path = UniPath.fromUnixString "."
    return $ LibManager.insNewNode (Library "Main" path ast PropertyMap.empty)
           $ LibManager.empty

mkEnv :: String -> IO Env
mkEnv code = do
    (libManager, libID) <- readCode code

    let defPoint = (DefPoint libID [Crumb.Module "Main", Crumb.Function "main" []])
    return $ Env.mk libManager 0 defPoint




runSession :: String -> Session () -> IO ()
runSession code session = do
    cfg <- Config.load
    env <- mkEnv code
    result <- Session.run cfg env session
    eitherStringToM $ fmapL Error.format result
