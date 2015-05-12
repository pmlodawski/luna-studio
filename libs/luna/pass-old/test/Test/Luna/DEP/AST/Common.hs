---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

module Test.Luna.DEP.AST.Common where

import           Flowbox.Control.Error
import           Flowbox.Prelude
import           Luna.DEP.AST.Module                                               (Module)
import           Luna.DEP.Data.ASTInfo                                             (ASTInfo)
import           Luna.DEP.Data.Source                                              (Source (Source))
import qualified Luna.DEP.Pass.Analysis.Alias.Alias                                as Analysis.Alias
import qualified Luna.DEP.Pass.Analysis.CallGraph.CallGraph                        as Analysis.CallGraph
import qualified Luna.DEP.Pass.Transform.AST.DepSort.DepSort                       as Transform.DepSort
import qualified Luna.DEP.Pass.Transform.AST.Desugar.ImplicitCalls.ImplicitCalls   as Desugar.ImplicitCalls
import qualified Luna.DEP.Pass.Transform.AST.Desugar.ImplicitScopes.ImplicitScopes as Desugar.ImplicitScopes
import qualified Luna.DEP.Pass.Transform.AST.Desugar.ImplicitSelf.ImplicitSelf     as Desugar.ImplicitSelf
import qualified Luna.DEP.Pass.Transform.AST.Desugar.TLRecUpdt.TLRecUpdt           as Desugar.TLRecUpdt
import qualified Luna.DEP.Pass.Transform.AST.TxtParser.TxtParser                   as TxtParser



getAST :: String -> IO (Module, ASTInfo)
getAST code = eitherStringToM' $ runEitherT $ do
    (ast, _, astInfo) <- EitherT $ TxtParser.run $ Source ["Main"] code
    (ast, astInfo)    <- EitherT $ Desugar.ImplicitSelf.run astInfo ast
    (ast, astInfo)    <- EitherT $ Desugar.TLRecUpdt.run astInfo ast
    aliasInfo         <- EitherT $ Analysis.Alias.run ast
    callGraph         <- EitherT $ Analysis.CallGraph.run aliasInfo ast
    ast               <- EitherT $ Transform.DepSort.run callGraph aliasInfo ast
    (ast, astInfo)    <- EitherT $ Desugar.ImplicitScopes.run astInfo aliasInfo ast
    EitherT $ Desugar.ImplicitCalls.run astInfo ast

