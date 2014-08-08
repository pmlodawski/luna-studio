---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
module Common where

import           Flowbox.Control.Error
import           Flowbox.Interpreter.Session.Data.DefPoint                               (DefPoint (DefPoint))
import           Flowbox.Interpreter.Session.Env                                         (Env)
import qualified Flowbox.Interpreter.Session.Env                                         as Env
import qualified Flowbox.Interpreter.Session.Error                                       as Error
import           Flowbox.Interpreter.Session.Session                                     (Session)
import qualified Flowbox.Interpreter.Session.Session                                     as Session
import qualified Flowbox.Luna.Data.AST.Crumb.Crumb                                       as Crumb
import           Flowbox.Luna.Data.Pass.Source                                           (Source (Source))
import qualified Flowbox.Luna.Data.PropertyMap                                           as PropertyMap
import           Flowbox.Luna.Lib.LibManager                                             (LibManager)
import qualified Flowbox.Luna.Lib.LibManager                                             as LibManager
import           Flowbox.Luna.Lib.Library                                                (Library (Library))
import qualified Flowbox.Luna.Lib.Library                                                as Library
import qualified Flowbox.Luna.Passes.Analysis.Alias.Alias                                as Analysis.Alias
import qualified Flowbox.Luna.Passes.Analysis.CallGraph.CallGraph                        as Analysis.CallGraph
import qualified Flowbox.Luna.Passes.Transform.AST.DepSort.DepSort                       as Transform.DepSort
import qualified Flowbox.Luna.Passes.Transform.AST.Desugar.ImplicitCalls.ImplicitCalls   as Desugar.ImplicitCalls
import qualified Flowbox.Luna.Passes.Transform.AST.Desugar.ImplicitScopes.ImplicitScopes as Desugar.ImplicitScopes
import qualified Flowbox.Luna.Passes.Transform.AST.Desugar.ImplicitSelf.ImplicitSelf     as Desugar.ImplicitSelf
import qualified Flowbox.Luna.Passes.Transform.AST.Desugar.TLRecUpdt.TLRecUpdt           as Desugar.TLRecUpdt
import qualified Flowbox.Luna.Passes.Transform.AST.TxtParser.TxtParser                   as TxtParser
import           Flowbox.Prelude
import qualified Flowbox.System.UniPath                                                  as UniPath



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
    env <- mkEnv code
    result <- Session.run env session
    eitherStringToM $ fmapL Error.format result
