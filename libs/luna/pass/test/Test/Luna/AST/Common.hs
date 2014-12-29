---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# LANGUAGE TupleSections #-}

module Test.Luna.AST.Common where

import           Flowbox.Control.Error
import           Flowbox.Prelude
import           Luna.AST.Module                                               (Module)
import           Luna.Data.Source                                              (Source (Source))
import qualified Luna.Pass.Analysis.Alias.Alias                                as Analysis.Alias
import qualified Luna.Pass.Analysis.CallGraph.CallGraph                        as Analysis.CallGraph
import qualified Luna.Pass.Transform.AST.DepSort.DepSort                       as Transform.DepSort
import qualified Luna.Pass.Transform.AST.Desugar.ImplicitCalls.ImplicitCalls   as Desugar.ImplicitCalls
import qualified Luna.Pass.Transform.AST.Desugar.ImplicitScopes.ImplicitScopes as Desugar.ImplicitScopes
import qualified Luna.Pass.Transform.AST.Desugar.ImplicitSelf.ImplicitSelf     as Desugar.ImplicitSelf
import qualified Luna.Pass.Transform.AST.Desugar.TLRecUpdt.TLRecUpdt           as Desugar.TLRecUpdt
import qualified Luna.Pass.Transform.AST.TxtParser.TxtParser                   as TxtParser



getAST :: String -> IO Module
getAST code = eitherStringToM' $ runEitherT $ do
    (ast, _, astInfo) <- EitherT $ TxtParser.run $ Source ["Main"] code
    (ast, astInfo)    <- EitherT $ Desugar.ImplicitSelf.run astInfo ast
    (ast, astInfo)    <- EitherT $ Desugar.TLRecUpdt.run astInfo ast
    aliasInfo         <- EitherT $ Analysis.Alias.run ast
    callGraph         <- EitherT $ Analysis.CallGraph.run aliasInfo ast
    ast               <- EitherT $ Transform.DepSort.run callGraph aliasInfo ast
    (ast, astInfo)    <- EitherT $ Desugar.ImplicitScopes.run astInfo aliasInfo ast
    (ast, _astInfo)   <- EitherT $ Desugar.ImplicitCalls.run astInfo ast
    return ast

